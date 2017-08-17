package maml.ast.jvm

import maml.ast._

import geotrellis.raster.Tile


case class TileLiteral(tile: Tile) extends Source {
  val kind = MamlKind.Tile
}
