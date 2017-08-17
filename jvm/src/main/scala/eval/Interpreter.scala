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
  def instructions(expression: Expression, children: Seq[Result]): Interpreted[Result]
  def apply(exp: Expression): Interpreted[Result]
}

case class NaiveInterpreter(directives: Seq[Directive]) {
  val fallbackDirective: Directive =
    { case (exp, res) => Invalid(NEL.of(UnhandledCase(exp, exp.kind))) }

  def instructions(expression: Expression, children: Seq[Result]): Interpreted[Result] =
    directives.reduceLeft(_ orElse _).orElse(fallbackDirective)((expression, children))

  def apply(exp: Expression): Interpreted[Result] = {
    val children = exp.children.map(apply).sequence
    children.andThen({ childRes => instructions(exp, childRes) })
  }
}

object Interpreter {
  def naive(directives: Directive*) = NaiveInterpreter(directives)
}

