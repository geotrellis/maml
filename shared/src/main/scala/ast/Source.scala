package com.azavea.maml.ast

import io.circe.Json
import io.circe.generic.JsonCodec

import java.util.UUID


trait Source extends Expression {
  val children: List[Expression] = List.empty
  def withChildren(children: List[Expression]) = this
  override val sources: List[Source] = List(this)
}

case class IntLiteral(value: Int) extends Source {
  val kind = MamlKind.Int
}

case class DoubleLiteral(value: Double) extends Source {
  val kind = MamlKind.Double
}

case class BoolLiteral(value: Boolean) extends Source {
  val kind = MamlKind.Bool
}

case class GeomJson(geojson: String) extends Source {
  val kind = MamlKind.Geom
}

trait UnboundSource extends Source

case class IntSource(id: String) extends UnboundSource {
  val kind = MamlKind.Int
}

case class DoubleSource(id: String) extends UnboundSource {
  val kind = MamlKind.Double
}

case class TileSource(id: String) extends UnboundSource {
  val kind = MamlKind.Tile
}

case class GeomSource(id: String) extends UnboundSource {
  val kind = MamlKind.Geom
}

case class BoolSource(id: String) extends UnboundSource {
  val kind = MamlKind.Bool
}
