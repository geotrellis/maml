package com.azavea.maml.ast

import geotrellis.raster.Tile

import java.util.UUID

case class TileLiteral(tile: Tile) extends Source {
  val kind = MamlKind.Tile
  def id = UUID.randomUUID.toString
}
