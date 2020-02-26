package endpoints.http4s.server

import endpoints.algebra.server.{DecodedUrl, EndpointsTestSuite}
import org.http4s.Uri

class ServerInterpreterUrlsTest extends EndpointsTestSuite[Endpoints] {

  val serverApi: Endpoints = new EndpointsTestApi()

  def decodeUrl[A](url: serverApi.Url[A])(rawValue: String): DecodedUrl[A] = {
    val uri = Uri.fromString(rawValue).right.get

    url.decodeUrl(uri) match {
      case None           => DecodedUrl.NotMatched
      case Some(Left(e))  => DecodedUrl.Malformed(e.toString :: Nil)
      case Some(Right(a)) => DecodedUrl.Matched(a)
    }
  }

  def serveEndpoint[Resp](endpoint: serverApi.Endpoint[_, Resp], response: => Resp)(runTests: Int => Unit): Unit = ???
}
