package endpoints.algebra.server

import endpoints.algebra
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{BeforeAndAfter, BeforeAndAfterAll}

import scala.concurrent.duration._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

trait ServerTestBase[T <: algebra.Endpoints] extends AnyWordSpec
  with Matchers
  with ScalaFutures
  with BeforeAndAfterAll
  with BeforeAndAfter {

  override implicit def patienceConfig: PatienceConfig = PatienceConfig(10.seconds, 10.millisecond)

  val serverApi: T

  /**
    * @param url An URL definition (e.g., `path / "foo"`)
    * @param urlCandidate An URL candidate (e.g., "/foo", "/bar")
    * @return Whether the URL candidate matched the URL definition, or not, or if
    *         decoding failed.
    */
  def decodeUrl[A](url: serverApi.Url[A])(urlCandidate: String): DecodedUrl[A]

  /**
    * @param runTests A function that is called after the server is started and before it is stopped. It takes
    *                 the TCP port number as parameter.
    */
  def serveEndpoint[Resp](endpoint: serverApi.Endpoint[_, Resp], response: => Resp)(runTests: Int => Unit): Unit

}

/**
  * @tparam A The result of decoding an URL candidate
  */
sealed trait DecodedUrl[+A] extends Serializable
object DecodedUrl {
  /** The URL candidate matched the given URL definition, and a `A` value was extracted from it */
  case class  Matched[+A](value: A)         extends DecodedUrl[A]
  /** The URL candidate didn’t match the given URL definition */
  case object NotMatched                    extends DecodedUrl[Nothing]
  /** The URL candidate matched the given URL definition, but the decoding process failed */
  case class Malformed(errors: Seq[String]) extends DecodedUrl[Nothing]
}
