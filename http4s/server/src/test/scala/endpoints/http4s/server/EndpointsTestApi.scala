package endpoints.http4s.server

import cats.effect.IO
import endpoints.algebra

class EndpointsTestApi
    extends Endpoints[IO]
    with BasicAuthentication
    with algebra.EndpointsTestApi
    with algebra.BasicAuthenticationTestApi
