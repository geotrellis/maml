package com.azavea.maml.ast

import io.circe.Json
import io.circe.generic.JsonCodec
import cats.effect.IO


trait Source extends Expression {
  val children: List[Expression] = List.empty
  def withChildren(children: List[Expression]) = this
  override val sources: Set[Source] = Set(this)
}

trait BoundSource extends Source {
  override val boundSources: Set[BoundSource] = Set(this)
  override val unboundSources: Set[UnboundSource] = Set()
}

trait UnboundSource extends Source {
  def resolveBinding: IO[BoundSource]
  override val boundSources: Set[BoundSource] = Set()
  override val unboundSources: Set[UnboundSource] = Set(this)
}

case class IntLiteral(value: Int) extends BoundSource {
  val kind = MamlKind.Int
}

case class DoubleLiteral(value: Double) extends BoundSource {
  val kind = MamlKind.Double
}

case class BoolLiteral(value: Boolean) extends BoundSource {
  val kind = MamlKind.Bool
}

case class GeoJson(geojson: String) extends BoundSource {
  val kind = MamlKind.Geom
}

