package maml.dsl

import maml.ast.jvm._

import geotrellis.raster.Tile


trait JvmLiterals {
  implicit def tileIsTileLiteral(tile: Tile): TileLiteral = TileLiteral(tile)
}
