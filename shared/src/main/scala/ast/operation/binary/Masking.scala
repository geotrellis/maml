package maml.ast.operation.binary

import maml.ast.operation.Operation
import maml.ast.MamlTree
import maml.ast.metadata._
import maml.ast.utility._

import io.circe._
import io.circe.generic.JsonCodec

import java.util.UUID

@JsonCodec
case class Point(x: Double, y: Double)

@JsonCodec
case class Polygon(points: Array[Point])

@JsonCodec
case class MultiPolygon(polygons: Array[Polygon])

case class Masking(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata], mask: MultiPolygon)
    extends Operation("mask")

