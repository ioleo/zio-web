package zio.web

import zio.Task

trait Client[MinMetadata[_], Ids] {
  final def invoke[M[+_] <: MinMetadata[_], I, O](endpoint: Endpoint[M, Unit, I, O])(input: I)(
    implicit ev: Ids <:< endpoint.Id
  ): Task[O] = invoke[M, Unit, I, O](endpoint)((), input)

  def invoke[M[+_] <: MinMetadata[_], P, I, O](endpoint: Endpoint[M, P, I, O])(params: P, input: I)(
    implicit ev: Ids <:< endpoint.Id
  ): Task[O]
}
