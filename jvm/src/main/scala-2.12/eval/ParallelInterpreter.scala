package com.azavea.maml.eval

import com.azavea.maml.ast._
import com.azavea.maml.error._
import com.azavea.maml.eval.directive._

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}
import cats.effect.ContextShift

import scala.reflect.ClassTag

class ParallelInterpreter[F[_]: Monad](directives: List[Directive])(
    implicit Par: Parallel[F],
    contextShift: ContextShift[F]
) extends Interpreter[F] {
  def apply(exp: Expression): F[Interpreted[Result]] = {
    val children = evalInF(exp)
    val out = children map {
      _.andThen({ childRes =>
        instructions(exp, childRes)
      })
    }
    out
  }

  def evalInF(
      expression: Expression
  )(implicit contextShift: ContextShift[F]): F[Interpreted[List[Result]]] = {
    val resultsF: F[List[Interpreted[Result]]] =
      expression.children parTraverse { expr =>
        apply(expr)
      }
    resultsF map { _.sequence }
  }

  val fallbackDirective: Directive = {
    case (exp, res) => Invalid(NEL.of(UnhandledCase(exp, exp.kind)))
  }

  def prependDirective(directive: Directive) =
    new ParallelInterpreter[F](directive +: directives)

  def appendDirective(directive: Directive) =
    new ParallelInterpreter[F](directives :+ directive)

  def instructions(
      expression: Expression,
      children: List[Result]
  ): Interpreted[Result] =
    directives
      .reduceLeft(_ orElse _)
      .orElse(fallbackDirective)((expression, children))
}

object ParallelInterpreter {
  def DEFAULT[T[_]](
      implicit P: Parallel[T],
      M: Monad[T],
      contextShift: ContextShift[T]
  ) =
    new ParallelInterpreter[T](NaiveInterpreter.DEFAULT.directives)
}
