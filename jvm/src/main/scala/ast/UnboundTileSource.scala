package com.azavea.maml.ast

import cats.effect._
import geotrellis.vector.Extent

import java.lang.IllegalStateException

trait UnboundTileSource extends UnboundSource {
  def resolveBinding: IO[BoundSource] = IO.raiseError {
    new IllegalStateException("Method 'resolveBinding' is not defined for tile based sources")
  }

  def resolveBindingForTmsTile(zoom: Int, x: Int, y: Int, buffer: Int)(implicit t: Timer[IO]): IO[BoundSource]
  def resolveBindingForExtent(zoom: Int, extent: Extent)(implicit t: Timer[IO]): IO[BoundSource]
}
