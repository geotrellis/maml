package com.azavea.maml.ast

import cats.effect._
import geotrellis.vector.Extent


trait UnboundTileSource extends UnboundSource {
  def resolveBinding: IO[BoundSource] =
    IO.raiseError(new java.lang.IllegalStateException("Method 'resolveBinding' is not defined for `UnboundTileSource`"))
  def resolveBindingForTmsTile(z: Int, x: Int, y: Int, buffer: Int)(implicit t: Timer[IO]): IO[BoundSource]
  def resolveBindingForExtent(extent: Extent, buffer: Int)(implicit t: Timer[IO]): IO[BoundSource]
}
