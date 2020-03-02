//package endpoints.http4s.server
//
//import cats.data.NonEmptyList
//import endpoints.algebra.BasicAuthentication.Credentials
//import org.http4s
//import org.http4s.headers.{Authorization, `WWW-Authenticate`}
//import org.http4s.{BasicCredentials, Challenge}
//
//trait BasicAuthentication
//    extends Endpoints
//    with endpoints.algebra.BasicAuthentication {
//
//  private val unauthorizedRequestResponse = http4s
//    .Response[Effect](Unauthorized)
//    .withHeaders(`WWW-Authenticate`(
//      NonEmptyList.of(Challenge("Basic", "Realm", Map("charset" -> "UTF-8")))))
//
//  private[endpoints] def basicAuthenticationHeader
//    : RequestHeaders[Credentials] =
//    headers =>
//      headers
//        .get(Authorization)
//        .map { authHeader =>
//          authHeader.credentials match {
//            case BasicCredentials(username, password) =>
//              Right(Credentials(username, password))
//            case _ => Left(unauthorizedRequestResponse)
//          }
//        }
//        .getOrElse(Left(unauthorizedRequestResponse))
//
//}
