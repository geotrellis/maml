package com.azavea.maml.eval

import com.azavea.maml.ast._

import geotrellis.vector.Extent
import cats._
import cats.implicits._
import cats.data._
import cats.effect._


object BindSourcesWithContext {
  def forTmsTile(expression: Expression)(implicit t: Timer[IO]): (Int, Int, Int) => IO[Expression] =
    (z: Int, x: Int, y: Int) => {
      def eval(expr: Expression, bindings: Map[UnboundSource, Source]): IO[Expression] = {
        expr match {
          case unboundTileSource: UnboundTileSource => IO.pure(bindings(unboundTileSource))
          case unboundSource: UnboundSource => IO.pure(bindings(unboundSource))
          case src: Source => IO.pure(src)
          case subExpression => eval(expr, bindings)
        }
      }

      for {
        bindings <- BindSources.getBindings(expression)
        evaluated <- eval(expression, bindings)
      } yield evaluated
    }

  def forExtent(expression: Expression)(implicit t: Timer[IO]): Extent => IO[Expression] =
    (extent: Extent) => {
      def eval(expr: Expression, bindings: Map[UnboundSource, Source]): IO[Expression] = {
        expr match {
          case unboundTileSource: UnboundTileSource => IO.pure(bindings(unboundTileSource))
          case unboundSource: UnboundSource => IO.pure(bindings(unboundSource))
          case src: Source => IO.pure(src)
          case subExpression => eval(expr, bindings)
        }
      }

      for {
        bindings <- BindSources.getBindings(expression)
        evaluated <- eval(expression, bindings)
      } yield evaluated
    }
}

