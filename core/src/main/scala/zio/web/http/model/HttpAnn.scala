package zio.web.http.model

import java.util.UUID

import zio.web.internal.Combine

sealed trait HttpAnn[+A]
sealed abstract class Method(val name: String) extends HttpAnn[Unit] {
  override def toString(): String = s"Method.$name"
}

object Method {

  // expose widened cases (simulating Scala 3 enums behavior) to help with type inference
  private object singleton {
    object OPTIONS extends Method("OPTIONS")
    object GET     extends Method("GET")
    object HEAD    extends Method("HEAD")
    object POST    extends Method("POST")
    object PUT     extends Method("PUT")
    object PATCH   extends Method("PATCH")
    object DELETE  extends Method("DELETE")
    object TRACE   extends Method("TRACE")
    object CONNECT extends Method("CONNECT")
  }

  val OPTIONS: Method = singleton.OPTIONS
  val GET: Method     = singleton.GET
  val HEAD: Method    = singleton.HEAD
  val POST: Method    = singleton.POST
  val PUT: Method     = singleton.PUT
  val PATCH: Method   = singleton.PATCH
  val DELETE: Method  = singleton.DELETE
  val TRACE: Method   = singleton.TRACE
  val CONNECT: Method = singleton.CONNECT

  def fromString(method: String): Method =
    method match {
      case "OPTIONS" => Method.OPTIONS
      case "GET"     => Method.GET
      case "HEAD"    => Method.HEAD
      case "POST"    => Method.POST
      case "PUT"     => Method.PUT
      case "PATCH"   => Method.PATCH
      case "DELETE"  => Method.DELETE
      case "TRACE"   => Method.TRACE
      case "CONNECT" => Method.CONNECT
      case _         => throw new IllegalArgumentException(s"Unable to handle method: $method")
    }
}

final case class Route[+A] private (path: Route.Path[A]) extends HttpAnn[A]

object Route {

  def apply[A](f: Path.Root => Path[A]): Route[A] = Route(f(Path.Root))

  /**
   * TODO: fiddle around with this encoding and see how the surface API "feels"
   *       can we get rid of `Root` and still have things infer nicely?
   */
  sealed trait Path[+A]

  object Path {

    sealed private[Route] trait Root extends Path[Unit]
    private[Route] object Root       extends Root

    final private case class Static(value: String) extends Path[Unit]

    final private case class Chain[A, B, C] private (left: Path[A], right: Path[B]) extends Path[C]

    private object Chain {

      def apply[A, B](left: Path[A], right: Path[B])(implicit c: Combine[A, B]): Path[c.Out] =
        Chain[A, B, c.Out](left, right)
    }

    final case class Param[A](from: String => A, to: A => String) extends Path[A] { self =>

      def derrive[B](map: A => B, contramap: B => A): Path[B] =
        Param[B](v => map(from(v)), v => to(contramap(v)))
    }

    val IntVal    = Param[Int](_.toInt, _.toString)
    val LongVal   = Param[Long](_.toLong, _.toString)
    val StringVal = Param[String](identity, identity)
    val UUIDVal   = Param[UUID](UUID.fromString, _.toString)

    implicit class PathOps[A](self: Path[A]) {

      final def /(segment: String): Path[A] =
        Path.Chain(self, Path.Static(segment))

      final def /[B](path: Path[B])(implicit c: Combine[A, B]): Path[c.Out] =
        Chain(self, path)
    }
  }
}
