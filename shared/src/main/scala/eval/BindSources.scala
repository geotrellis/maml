package com.azavea.maml.eval

import com.azavea.maml.ast._
import cats._
import cats.implicits._
import cats.data._
import cats.effect._


object BindSources {
  def getBindings(expression: Expression)(implicit t: Timer[IO]): IO[Map[UnboundSource, Source]] =
    expression.unboundSources.map { u =>
      u.resolveBinding.map { res => (u -> res) }
    }.toList
    .parSequence
    .map(_.toMap)

  def naive(expression: Expression)(implicit t: Timer[IO]): IO[Expression] = {
    def eval(expr: Expression, bindings: Map[UnboundSource, Source]): IO[Expression] = {
      expr match {
        case unboundSource: UnboundSource => IO.pure(bindings(unboundSource))
        case src: Source => IO.pure(src)
        case subExpression => eval(expr, bindings)
      }
    }

    for {
      bindings <- getBindings(expression)
      evaluated <- eval(expression, bindings)
    } yield evaluated
  }
}

