package com.azavea.maml.eval

import com.azavea.maml.ast._

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}

import scala.reflect.ClassTag


case class NaiveInterpreter(directives: List[Directive]) extends Interpreter {

  def prependDirective(directive: Directive): Interpreter =
    NaiveInterpreter(directive +: directives)

  def appendDirective(directive: Directive): Interpreter =
    NaiveInterpreter(directives :+ directive)

  val fallbackDirective: Directive =
    { case (exp, res) => Invalid(NEL.of(UnhandledCase(exp, exp.kind))) }

  def instructions(expression: Expression, children: List[Result]): Interpreted[Result] =
    directives.reduceLeft(_ orElse _).orElse(fallbackDirective)((expression, children))
}

