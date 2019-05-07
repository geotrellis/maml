package com.azavea.maml.eval

import com.azavea.maml.ast._
import com.azavea.maml.error._
import com.azavea.maml.eval.directive._

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}
import cats.effect.{Concurrent, Fiber}

import scala.reflect.ClassTag

class ParallelInterpreter[F[_]](directives: List[Directive])(
    implicit Conc: Concurrent[F]
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

  def evalInF(expression: Expression): F[Interpreted[List[Result]]] = {
    val fibsF: F[List[Fiber[F, Interpreted[Result]]]] =
      expression.children traverse { expr =>
        Conc.start(apply(expr))
      }
    fibsF flatMap { _.traverse { _.join } } map { _.sequence }
  }

  val fallbackDirective: Directive = {
    case (exp, res) => Invalid(NEL.of(UnhandledCase(exp, exp.kind)))
  }

  def instructions(
      expression: Expression,
      children: List[Result]
  ): Interpreted[Result] =
    directives
      .reduceLeft(_ orElse _)
      .orElse(fallbackDirective)((expression, children))
}

object ParallelInterpreter {
  def DEFAULT[T[_]: Concurrent] =
    new ParallelInterpreter[T](NaiveInterpreter.DEFAULT.directives :+ OpDirectives.sleep)
}
