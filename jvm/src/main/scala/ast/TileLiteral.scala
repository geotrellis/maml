package com.azavea.maml.ast

import com.azavea.maml.eval.tile._

import geotrellis.raster.Tile

import java.util.UUID

case class TileLiteral(tile: Tile) extends Source {
  val kind = MamlKind.Tile
}

case class LazyTileLiteral(tile: LazyTile) extends Source {
  val kind = MamlKind.Tile
}

