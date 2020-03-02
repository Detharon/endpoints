package endpoints.http4s.server

import cats.effect.{IO, Sync}
import endpoints.algebra

class EndpointsTestApi
    extends Endpoints
//    with BasicAuthentication
//    with algebra.BasicAuthenticationTestApi
    with algebra.EndpointsTestApi {

  type Effect[A] = IO[A]

  implicit def Effect: Sync[IO] = IO.ioEffect
}
