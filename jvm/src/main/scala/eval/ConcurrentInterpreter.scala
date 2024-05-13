package com.azavea.maml.eval

import com.azavea.maml.ast._
import com.azavea.maml.error._
import com.azavea.maml.eval.directive._

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}
import cats.effect.{Fiber, Spawn}

import scala.reflect.ClassTag

class ConcurrentInterpreter[F[_]](directives: List[Directive])(implicit
  Conc: Spawn[F]
) extends Interpreter[F] {
  def apply(exp: Expression): F[Interpreted[Result]] = {
    val children = evalInF(exp)
    val out = children.map {
      _.andThen { childRes =>
        instructions(exp, childRes)
      }
    }
    out
  }

  def evalInF(expression: Expression): F[Interpreted[List[Result]]] = {
    val fibsF: F[List[Fiber[F, Throwable, Interpreted[Result]]]] = expression.children.traverse { expr => Conc.start(apply(expr)) }
    fibsF.flatMap { _.traverse { _.joinWithNever } }.map { _.sequence }
  }

  val fallbackDirective: Directive = { case (exp, res) =>
    Invalid(NEL.of(UnhandledCase(exp, exp.kind)))
  }

  def prependDirective(directive: Directive) =
    new ConcurrentInterpreter[F](directive +: directives)

  def appendDirective(directive: Directive) =
    new ConcurrentInterpreter[F](directives :+ directive)

  def instructions(
    expression: Expression,
    children: List[Result]
  ): Interpreted[Result] =
    directives
      .reduceLeft(_ orElse _)
      .orElse(fallbackDirective)((expression, children))
}

object ConcurrentInterpreter {
  def DEFAULT[T[_]: Spawn] =
    new ConcurrentInterpreter[T](NaiveInterpreter.DEFAULT.directives)
}
