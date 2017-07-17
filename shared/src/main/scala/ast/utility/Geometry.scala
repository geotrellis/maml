package maml.ast.utility

import io.circe.generic.JsonCodec

@JsonCodec
case class Point(x: Double, y: Double)

@JsonCodec
case class Polygon(points: Array[Point])

@JsonCodec
case class MultiPolygon(polygons: Array[Polygon])

