package com.azavea.maml.ast

import com.azavea.maml.eval.tile._

import geotrellis.raster._

import java.util.UUID

case class TileLiteral(tile: Tile, extent: RasterExtent) extends Source {
  val kind = MamlKind.Tile
}

case class LazyTileLiteral(tile: LazyTile) extends Source {
  val kind = MamlKind.Tile
}

