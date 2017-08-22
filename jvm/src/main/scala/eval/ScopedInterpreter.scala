package maml.eval

import maml.ast._
import maml.error._

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}
import geotrellis.raster.GridBounds


trait ScopedInterpreter[Scope] {
  def scopeTo(exp: Expression, previous: Option[Scope]): Scope
  def fallbackDirective: ScopedDirective[Scope]
  def instructions(expression: Expression, children: Seq[Result], scope: Scope): Interpreted[Result]
  def apply(exp: Expression, scope: Option[Scope]): Interpreted[Result]
}

