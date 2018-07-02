package com.azavea.maml.spark.ast

import com.azavea.maml.ast._

import geotrellis.spark._


/** An Expression leaf node that contains a GeoTrellis layer of Tiles. */
case class RDDLiteral(value: TileLayerRDD[SpatialKey]) extends BoundSource {
  val kind = MamlKind.Tile
}

