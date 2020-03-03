package endpoints.http4s.server

import cats.data.NonEmptyList
import cats.syntax.functor._
import endpoints.{Tupler, Valid}
import endpoints.algebra.BasicAuthentication.Credentials
import endpoints.algebra.Documentation
import org.http4s
import org.http4s.headers.{Authorization, `WWW-Authenticate`}
import org.http4s.{BasicCredentials, Challenge}

trait BasicAuthentication
    extends Endpoints
    with endpoints.algebra.BasicAuthentication {

  private val unauthorizedRequestResponse = http4s
    .Response[Effect](Unauthorized)
    .withHeaders(`WWW-Authenticate`(
      NonEmptyList.of(Challenge("Basic", "Realm", Map("charset" -> "UTF-8")))))

  private[endpoints] def basicAuthenticationHeader: RequestHeaders[Option[Credentials]] =
    headers =>
      Valid(
        headers
          .get(Authorization)
          .flatMap { authHeader =>
            authHeader.credentials match {
              case BasicCredentials(username, password) =>
                Some(Credentials(username, password))
              case _ => None
            }
          }
      )

  def authenticatedRequest[U, E, H, UE, HC, Out](
    method: Method,
    url: Url[U],
    entity: RequestEntity[E],
    headers: RequestHeaders[H],
    requestDocs: Documentation = None
  )(implicit
    tuplerUE: Tupler.Aux[U, E, UE],
    tuplerHC: Tupler.Aux[H, Credentials, HC],
    tuplerUEHC: Tupler.Aux[UE, HC, Out]
  ): Request[Out] =
    extractUrlAndHeaders(method, url, headers ++ basicAuthenticationHeader) {
      case (_, (_, None)) =>
        _ => Left(unauthorizedRequestResponse)
      case (u, (h, Some(credentials))) =>
        http4sRequest =>
          Right(
            entity(http4sRequest)
              .map(e => tuplerUEHC(tuplerUE(u, e), tuplerHC(h, credentials)))
          )
    }

}
