package maml.eval

import maml.ast._
import maml.error._

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}

import scala.reflect.ClassTag


case class NaiveInterpreter(directives: List[Directive]) {
  val fallbackDirective: Directive =
    { case (exp, res) => Invalid(NEL.of(UnhandledCase(exp, exp.kind))) }

  def instructions(expression: Expression, children: List[Result]): Interpreted[Result] =
    directives.reduceLeft(_ orElse _).orElse(fallbackDirective)((expression, children))

  def apply(exp: Expression): Interpreted[Result] = {
    val children: Interpreted[List[Result]] = exp.children.map(apply).sequence
    children.andThen({ childRes => instructions(exp, childRes) })
  }
}

