package com.azavea.maml.ast

import io.circe.Json
import io.circe.generic.JsonCodec
import cats.effect.IO


trait Source extends Expression {
  val children: List[Expression] = List.empty
  def withChildren(children: List[Expression]) = this
}
trait Literal extends Source
trait Variable extends Source

case class IntLit(value: Int) extends Literal {
  val kind = MamlKind.Int
}

case class IntVar(name: String) extends Variable {
  val kind = MamlKind.Int
}

case class DblLit(value: Double) extends Literal {
  val kind = MamlKind.Double
}

case class DblVar(name: String) extends Variable {
  val kind = MamlKind.Double
}

case class BoolLit(value: Boolean) extends Literal {
  val kind = MamlKind.Bool
}

case class BoolVar(name: String) extends Variable {
  val kind = MamlKind.Double
}

case class GeomLit(geojson: String) extends Literal {
  val kind = MamlKind.Geom
}

case class GeomVar(name: String) extends Variable {
  val kind = MamlKind.Geom
}

case class RasterLit[A](raster: A) extends Literal {
  val kind = MamlKind.Tile
}

case class RasterVar(name: String) extends Variable {
  val kind = MamlKind.Tile
}

