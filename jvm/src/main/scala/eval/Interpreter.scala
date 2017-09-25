package com.azavea.maml.eval

import com.azavea.maml.ast._
import com.azavea.maml.error._

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}

import scala.reflect.ClassTag


trait Interpreter {
  def fallbackDirective: Directive
  def instructions(expression: Expression, children: List[Result]): Interpreted[Result]
  def apply(exp: Expression): Interpreted[Result] = {
    val children: Interpreted[List[Result]] = exp.children.map(apply).sequence
    children.andThen({ childRes => instructions(exp, childRes) })
  }
}

object Interpreter {
  def naive(directives: Directive*) = NaiveInterpreter(directives.toList)

  def buffering(directives: ScopedDirective[BufferingInterpreter.Scope]*) = BufferingInterpreter(directives.toList)
}

