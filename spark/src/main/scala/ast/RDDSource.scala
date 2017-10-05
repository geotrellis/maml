package com.azavea.maml.spark.ast

import com.azavea.maml.ast._

import geotrellis.spark._


/** An Expression leaf node that contains a GeoTrellis layer of Tiles. */
case class RDDLiteral(value: TileLayerRDD[SpatialKey]) extends Source {
  val kind = MamlKind.Tile
}

/** An unbound source to be realized into an [[RDDLiteral]]. */
case class RDDSource(id: String) extends UnboundSource {
  val kind = MamlKind.Tile
}
