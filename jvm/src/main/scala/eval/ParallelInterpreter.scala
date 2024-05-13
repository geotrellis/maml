package com.azavea.maml.eval

import com.azavea.maml.ast._
import com.azavea.maml.error._

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}
import cats.effect.kernel.Async

class ParallelInterpreter[F[_]: Monad: Parallel, G[_]](directives: List[Directive]) extends Interpreter[F] {
  def apply(exp: Expression): F[Interpreted[Result]] = {
    val children = evalInF(exp)
    val out = children.map(_.andThen(instructions(exp, _)))
    out
  }

  def evalInF(
    expression: Expression
  ): F[Interpreted[List[Result]]] = {
    val resultsF: F[List[Interpreted[Result]]] = expression.children.parTraverse { expr => apply(expr) }
    resultsF.map { _.sequence }
  }

  val fallbackDirective: Directive = { case (exp, res) =>
    Invalid(NEL.of(UnhandledCase(exp, exp.kind)))
  }

  def prependDirective(directive: Directive) =
    new ParallelInterpreter[F, G](directive +: directives)

  def appendDirective(directive: Directive) =
    new ParallelInterpreter[F, G](directives :+ directive)

  def instructions(
    expression: Expression,
    children: List[Result]
  ): Interpreted[Result] =
    directives
      .reduceLeft(_ orElse _)
      .orElse(fallbackDirective)((expression, children))
}

object ParallelInterpreter {
  def DEFAULT[T[_]: Parallel: Monad, U[_]] = new ParallelInterpreter[T, U](NaiveInterpreter.DEFAULT.directives)
}
