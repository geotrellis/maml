package com.azavea.maml.eval

import com.azavea.maml.ast._
import com.azavea.maml.error._

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}
import geotrellis.raster.GridBounds


trait ScopedInterpreter[Scope] extends Interpreter {
  def scopeFor(exp: Expression, previous: Option[Scope]): Scope
  def appendDirective(directive: ScopedDirective[Scope]): ScopedInterpreter[Scope]
  def prependDirective(directive: ScopedDirective[Scope]): ScopedInterpreter[Scope]
  def fallbackDirective: ScopedDirective[Scope]
  def instructions(expression: Expression, children: Seq[Result], scope: Scope): Interpreted[Result]

  def apply(exp: Expression): Interpreted[Result] = {
    def eval(exp: Expression, maybeScope: Option[Scope] = None): Interpreted[Result] = {
      val currentScope = scopeFor(exp, maybeScope)
      val children: Interpreted[List[Result]] = exp.children.map({ childTree =>
        val childScope = scopeFor(childTree, Some(currentScope))
        eval(childTree, Some(childScope))
      }).sequence
      children.andThen({ childResult => instructions(exp, childResult, currentScope) })
    }
    eval(exp)
  }
}

