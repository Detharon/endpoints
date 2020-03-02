package endpoints.http4s.server

import endpoints.{Invalid, Valid}
import endpoints.algebra.server.{DecodedUrl, EndpointsTestSuite}
import org.http4s.Uri

class ServerInterpreterUrlsTest extends EndpointsTestSuite[EndpointsTestApi] {

  val serverApi = new EndpointsTestApi()

  def decodeUrl[A](url: serverApi.Url[A])(rawValue: String): DecodedUrl[A] = {
    val uri = Uri.fromString(rawValue).getOrElse(sys.error(s"Illegal URI: $rawValue"))

    url.decodeUrl(uri) match {
      case None                  => DecodedUrl.NotMatched
      case Some(Invalid(errors)) => DecodedUrl.Malformed(errors)
      case Some(Valid(a))        => DecodedUrl.Matched(a)
    }
  }

  def serveEndpoint[Resp](endpoint: serverApi.Endpoint[_, Resp], response: => Resp)(runTests: Int => Unit): Unit = ???
}
