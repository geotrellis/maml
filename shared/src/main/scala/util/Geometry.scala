package com.azavea.maml.util

import io.circe.generic.JsonCodec


@JsonCodec
case class MamlPoint(x: Double, y: Double)

@JsonCodec
case class MamlPolygon(points: Array[MamlPoint])

@JsonCodec
case class MamlMultiPolygon(polygons: Array[MamlPolygon])

