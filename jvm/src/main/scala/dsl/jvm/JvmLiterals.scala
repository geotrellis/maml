package maml.dsl

import maml.ast.jvm._

import geotrellis.raster.Tile

// TODO Remove?
trait JvmLiterals {
  // TODO: This implicit conversion isn't possible, unless TileLiteral's `T`
  // is contrained to `Monoid` so we can `mempty` here.
  // implicit def tileIsTileLiteral(tile: Tile): TileLiteral = TileLiteral(tile)
}
