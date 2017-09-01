package maml.ast.jvm

import maml.ast._

import geotrellis.raster.Tile

import java.util.UUID

case class TileLiteral[T](tile: Tile, extra: T) extends Source[T] {
  val kind = MamlKind.Tile
  def id = UUID.randomUUID.toString
}
