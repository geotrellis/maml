package maml.rdd.ast

import maml.ast._

import geotrellis.spark._


case class SpatialRDDLiteral(value: TileLayerRDD[SpatialKey]) extends Source {
  val id = value.toString
  val kind = MamlKind.Tile
}

case class SpatialRDDSource(id: String) extends UnboundSource {
  val kind = MamlKind.Tile
}
