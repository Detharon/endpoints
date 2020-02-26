package endpoints.http4s.server

import cats.effect.Sync
import cats.implicits._
import endpoints.algebra.Documentation
import endpoints.{InvariantFunctor, PartialInvariantFunctor, Semigroupal, Tupler, Validated, algebra}
import fs2._
import org.http4s
import org.http4s.{EntityEncoder, Header, Headers}

import scala.language.higherKinds

trait Endpoints extends algebra.Endpoints with EndpointsWithCustomErrors with BuiltInErrors

trait EndpointsWithCustomErrors extends algebra.EndpointsWithCustomErrors with Methods with Urls {
  type Effect[A]
  implicit def Effect: Sync[Effect]

  type RequestHeaders[A] = http4s.Headers => Either[ErrorResponse, A]

  type Request[A] =
    PartialFunction[http4s.Request[Effect], Either[ErrorResponse, Effect[A]]]

  type RequestEntity[A] = http4s.Request[Effect] => Effect[A]

  type Response[A] = A => http4s.Response[Effect]

  type ResponseEntity[A] = http4s.EntityEncoder[Effect, A]

  type ResponseHeaders[A] = A => http4s.Headers

  case class Endpoint[A, B](request: Request[A], response: Response[B]) {
    def implementedBy(implementation: A => B)
      : PartialFunction[http4s.Request[Effect], Effect[http4s.Response[Effect]]] = {
      case req: http4s.Request[Effect] if request.isDefinedAt(req) =>
        request(req) match {
          case Right(a)            => a.map(implementation).map(response)
          case Left(errorResponse) => errorResponse.pure[Effect]
        }
    }

    def implementedByEffect(implementation: A => Effect[B]): PartialFunction[http4s.Request[Effect], Effect[http4s.Response[Effect]]] = {
      case req: http4s.Request[Effect] if request.isDefinedAt(req) =>
        request(req) match {
          case Right(a)            => a.flatMap(implementation).map(response)
          case Left(errorResponse) => errorResponse.pure[Effect]
        }
    }
  }

  def routesFromEndpoints(endpoints: PartialFunction[http4s.Request[Effect], Effect[http4s.Response[Effect]]]*) =
    endpoints.reduceLeft(_ orElse _)

  /**
    * HEADERS
    */
  def emptyHeaders: RequestHeaders[Unit] = _ => Right(())

  def header(name: String, docs: Documentation): RequestHeaders[String] =
    headers =>
      headers.filter(_.name.value == name).collectFirst {
        case h => h.name.value
      } match {
        case Some(value) => Right(value)
        case None        => Left(badRequestResponse)
    }

  def optHeader(name: String,
                docs: Documentation): RequestHeaders[Option[String]] =
    headers =>
      headers.filter(_.name.value == name).collectFirst {
        case h => Some(h.name.value)
      } match {
        case Some(value) => Right(value)
        case None        => Left(badRequestResponse)
    }

  /**
    * RESPONSES
    */
  implicit lazy val responseInvFunctor: endpoints.InvariantFunctor[Response] =
    new endpoints.InvariantFunctor[Response] {
      def xmap[A, B](fa: Response[A], f: A => B, g: B => A): Response[B] = fa compose g
    }

  def response[A, B, R](statusCode: StatusCode, entity: ResponseEntity[A],
                                 docs: Documentation, headers: ResponseHeaders[B])
                                (implicit tupler: Tupler.Aux[A, B, R]): Response[R] =
    r => {
      val (a, b) = tupler.unapply(r)
      http4s.Response[Effect](status = statusCode, headers = headers(b) ++ entity.headers, body = entity.toEntity(a).body)
    }

  def choiceResponse[A, B](responseA: Response[A], responseB: Response[B]): Response[Either[A, B]] = {
    case Left(a) => responseA(a)
    case Right(b) => responseB(b)
  }

  def emptyResponse: ResponseEntity[Unit] =
    EntityEncoder.emptyEncoder[Effect, Unit]

  def textResponse: ResponseEntity[String] =
    EntityEncoder.stringEncoder

  implicit def responseHeadersSemigroupal: Semigroupal[ResponseHeaders] =
    new Semigroupal[ResponseHeaders] {
      def product[A, B](fa: ResponseHeaders[A], fb: ResponseHeaders[B])(implicit tupler: Tupler[A, B]): ResponseHeaders[tupler.Out] =
        out => {
          val (a, b) = tupler.unapply(out)
          fa(a) ++ fb(b)
        }
    }

  implicit def responseHeadersInvFunctor: PartialInvariantFunctor[ResponseHeaders] =
    new PartialInvariantFunctor[ResponseHeaders] {
      def xmapPartial[A, B](fa: ResponseHeaders[A], f: A => Validated[B], g: B => A): ResponseHeaders[B] =
        fa compose g
    }

  def emptyResponseHeaders: ResponseHeaders[Unit] =
    _ => Headers.empty

  def responseHeader(name: String, docs: Documentation = None): ResponseHeaders[String] =
    value => Headers.of(Header(name, value))

  def optResponseHeader(name: String, docs: Documentation = None): ResponseHeaders[Option[String]] = {
    case Some(value) => responseHeader(name, docs)(value)
    case None => emptyResponseHeaders(())
  }

  def endpoint[A, B](request: Request[A],
                     response: Response[B],
                     docs: EndpointDocs = EndpointDocs()): Endpoint[A, B] =
    Endpoint(request, response)



  /**
    * REQUESTS
    */
  def emptyRequest: RequestEntity[Unit] = _ => ().pure[Effect]

  def textRequest: RequestEntity[String] =
    req => req.body.through(text.utf8Decode).compile.toList.map(_.mkString)

  def request[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, Out](
      method: Method,
      url: Url[UrlP],
      entity: RequestEntity[BodyP] = emptyRequest,
      docs: Documentation = None,
      headers: RequestHeaders[HeadersP] = emptyHeaders
  )(implicit tuplerUB: Tupler.Aux[UrlP, BodyP, UrlAndBodyPTupled],
    tuplerUBH: Tupler.Aux[UrlAndBodyPTupled, HeadersP, Out]): Request[Out] =
    Function.unlift(
      req =>
        if (req.method == method) {
          url
            .decodeUrl(req.uri)
            .map(_.flatMap(u =>
              headers(req.headers).map(h =>
                entity(req).map(body => tuplerUBH(tuplerUB(u, body), h)))))
        } else
        None)

  implicit def reqEntityInvFunctor: endpoints.InvariantFunctor[RequestEntity] =
    new InvariantFunctor[RequestEntity] {
      override def xmap[From, To](
          f: http4s.Request[Effect] => Effect[From],
          map: From => To,
          contramap: To => From): http4s.Request[Effect] => Effect[To] =
        body => f(body).map(map)
    }

  implicit def reqHeadersInvFunctor
    : endpoints.InvariantFunctor[RequestHeaders] =
    new InvariantFunctor[RequestHeaders] {
      override def xmap[From, To](
          f: Headers => Either[ErrorResponse, From],
          map: From => To,
          contramap: To => From): Headers => Either[ErrorResponse, To] =
        headers => f(headers).map(map)
    }

  implicit def reqHeadersSemigroupal: endpoints.Semigroupal[RequestHeaders] =
    new Semigroupal[RequestHeaders] {
      override def product[A, B](fa: Headers => Either[ErrorResponse, A],
                                 fb: Headers => Either[ErrorResponse, B])(
          implicit tupler: Tupler[A, B])
        : Headers => Either[ErrorResponse, tupler.Out] =
        headers =>
          fa(headers)
            .flatMap(a => fb(headers).map(b => tupler(a, b)))
    }
}
