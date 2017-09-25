package com.azavea.maml.ast.jvm

import com.azavea.maml.ast._

import geotrellis.raster.Tile

import java.util.UUID

case class TileLiteral(tile: Tile) extends Source {
  val kind = MamlKind.Tile
  def id = UUID.randomUUID.toString
}
