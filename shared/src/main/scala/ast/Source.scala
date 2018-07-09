package com.azavea.maml.ast

import com.azavea.maml.error._

import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import io.circe.Json
import io.circe.generic.JsonCodec
import cats.effect.IO

import java.lang.IllegalArgumentException


trait Source extends Expression {
  val children: List[Expression] = List.empty
  def withChildren(children: List[Expression]) = this
}
trait Literal extends Source
trait Variable extends Source {
  def name: String
  override def varMap: Map[String, MamlKind] = Map(name -> kind)

  override def bind(args: Map[String, Literal]): ValidatedNel[MamlError, Expression] =
    args.get(name) match {
      case Some(arg) => Valid(arg)
      case None => Invalid(NEL.of(NoVariableBinding(this, args)))
    }
}

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
  val kind = MamlKind.Bool
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

