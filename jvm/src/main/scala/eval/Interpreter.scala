package maml.eval

import maml.ast._
import maml.error._

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}

import scala.reflect.ClassTag


trait Interpreter {
  def fallbackDirective: Directive
  def instructions(expression: Expression, children: List[Result]): Interpreted[Result]
  def apply(exp: Expression): Interpreted[Result]
}

object Interpreter {
  def naive(directives: Directive*) = NaiveInterpreter(directives.toList)

  def buffering(directives: ScopedDirective[BufferingInterpreter.Scope]*) = BufferingInterpreter(directives.toList)
}

