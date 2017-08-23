package maml.eval

import maml.ast._
import maml.error._

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
  def scopeTo(exp: Expression, previous: Option[BufferingInterpreter.Scope]): BufferingInterpreter.Scope = {
    val scope = previous.getOrElse(BufferingInterpreter.Scope(0, options.tileSize))
    exp match {
      case f: FocalExpression => scope.copy(buffer = scope.buffer + f.neighborhood.extent)
      case _ => scope
    }
  }

  val fallbackDirective: ScopedDirective[BufferingInterpreter.Scope] =
    { case (exp, res, scope) => Invalid(NEL.of(UnhandledCase(exp, exp.kind))) }

  def instructions(expression: Expression, children: Seq[Result], scope: BufferingInterpreter.Scope): Interpreted[Result] =
    directives.reduceLeft(_ orElse _).orElse(fallbackDirective)((expression, children, scope))

  def apply(exp: Expression, maybeScope: Option[BufferingInterpreter.Scope] = None): Interpreted[Result] = {
    val scope = scopeTo(exp, maybeScope)
    val children: Interpreted[List[Result]] = exp.children.map({ childTree =>
      val childScope = scopeTo(childTree, Some(scope))
      apply(childTree, Some(childScope))
    }).sequence

    children.andThen({ childRes => instructions(exp, childRes, scope) })
  }
}


object BufferingInterpreter {
  case class Options(tileSize: Int)
  case class Scope(buffer: Int, tileSize: Int)

  def gridbounds(expectedTileSize: Int, buffer: Int, extent: Int): GridBounds =
    GridBounds(extent, extent, expectedTileSize - 1 + buffer * 2 + extent, expectedTileSize - 1 + buffer * 2 + extent)
}

