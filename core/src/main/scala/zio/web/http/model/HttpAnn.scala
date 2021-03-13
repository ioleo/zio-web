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

sealed trait Route[+A] extends HttpAnn[A]

object Route {
  def apply[A](f: Root => Route[A]): Route[A] = f(Root)

  sealed private[http] trait Root extends Route[Unit]
  private[http] object Root       extends Root

  final private[http] case class Static(value: String) extends Route[Unit]

  final private[http] case class Chain[L, R, P] private (
    left: Route[L],
    right: Route[R],
    combine: Combine.Aux[L, R, P]
  ) extends Route[P]

  private object Chain {

    def apply[L, R, P](left: Route[L], right: Route[R])(implicit c: Combine[L, R]): Route[c.Out] =
      Chain(left, right, c.asInstanceOf[Combine.Aux[L, R, c.Out]])
  }

  final case class Param[A](from: String => A, to: A => String) extends Route[A] {

    def derive[B](map: A => B, contramap: B => A): Route[B] =
      Param[B](v => map(from(v)), v => to(contramap(v)))
  }

  val IntVal    = Param[Int](_.toInt, _.toString)
  val LongVal   = Param[Long](_.toLong, _.toString)
  val StringVal = Param[String](identity, identity)
  val UUIDVal   = Param[UUID](UUID.fromString, _.toString)

  implicit class RouteOps[L](self: Route[L]) {

    final def /(segment: String): Route[L] =
      Chain[L, Unit, L](self, Static(segment))

    final def /[R](that: Route[R])(implicit c: Combine[L, R]): Route[c.Out] =
      Chain[L, R, c.Out](self, that)
  }
}
