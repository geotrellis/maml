package maml.ast.jvm

import maml.ast._

import geotrellis.raster.Tile

import java.util.UUID

case class TileLiteral(tile: Tile) extends Source {
  val kind = MamlKind.Tile
  def id = UUID.randomUUID.toString
}
