package com.azavea.maml.eval

import com.azavea.maml.ast._
import com.azavea.maml.util._
import com.azavea.maml.error._
import com.azavea.maml.eval.directive._

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}
import geotrellis.raster.GridBounds

import scala.reflect.ClassTag


case class BufferingInterpreter(
  directives: List[ScopedDirective[BufferingInterpreter.Scope]],
  options: BufferingInterpreter.Options = BufferingInterpreter.Options(256)
) extends ScopedInterpreter[BufferingInterpreter.Scope] {

  def scopeFor(exp: Expression, previous: Option[BufferingInterpreter.Scope]): BufferingInterpreter.Scope = {
    val scope = previous.getOrElse(BufferingInterpreter.Scope(0, options.tileSize))
    exp match {
      case f: FocalExpression =>
        val n = NeighborhoodConversion(f.neighborhood)
        scope.copy(buffer = scope.buffer + n.extent)
      case _ => scope
    }
  }

  val fallbackDirective: ScopedDirective[BufferingInterpreter.Scope] =
    { case (exp, res, scope) => Invalid(NEL.of(UnhandledCase(exp, exp.kind))) }

  def instructions(expression: Expression, children: Seq[Result], scope: BufferingInterpreter.Scope): Interpreted[Result] =
    directives.reduceLeft(_ orElse _).orElse(fallbackDirective)((expression, children, scope))
}


object BufferingInterpreter {
  case class Options(tileSize: Int)
  case class Scope(buffer: Int, tileSize: Int)

  def gridbounds(expectedTileSize: Int, buffer: Int, extent: Int): GridBounds =
    GridBounds(extent, extent, expectedTileSize - 1 + buffer * 2 + extent, expectedTileSize - 1 + buffer * 2 + extent)
}
