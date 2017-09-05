package maml.eval

import maml.ast._
import maml.error._

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}
import geotrellis.raster.GridBounds


trait ScopedInterpreter[Scope, T] {
  def scopeFor(exp: Expression[T], previous: Option[Scope]): Scope
  def fallbackDirective: ScopedDirective[Scope, T]
  def instructions(expression: Expression[T], children: Seq[Result], scope: Scope): Interpreted[Result]

  def apply(exp: Expression[T], maybeScope: Option[Scope] = None): Interpreted[Result] = {
    val currentScope = scopeFor(exp, maybeScope)
    val children: Interpreted[List[Result]] = exp.children.map({ childTree =>
      val childScope = scopeFor(childTree, Some(currentScope))
      apply(childTree, Some(childScope))
    }).sequence

    children.andThen({ childResult => instructions(exp, childResult, currentScope) })
  }
}
