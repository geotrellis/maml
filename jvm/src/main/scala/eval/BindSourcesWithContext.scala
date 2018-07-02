package com.azavea.maml.eval

import com.azavea.maml.ast._
import com.azavea.maml.util.NeighborhoodConversion

import geotrellis.vector.Extent
import cats._
import cats.implicits._
import cats.data._
import cats.effect._


object BindSourcesWithContext {
  def forTmsTile(expression: Expression)(implicit t: Timer[IO]): (Int, Int, Int) => IO[Expression] =
    (z: Int, x: Int, y: Int) => {
      def eval(expr: Expression, buffer: Int): IO[Expression] = {
        expr match {
          case unboundTileSource: UnboundTileSource =>
            unboundTileSource.resolveBindingForTmsTile(z, x, y, buffer)
          case unboundSource: UnboundSource =>
            unboundSource.resolveBinding
          case boundSource: BoundSource =>
            IO.pure(boundSource)
          case focal: FocalExpression =>
            focal.children
              .map(eval(_, buffer + NeighborhoodConversion(focal.neighborhood).extent))
              .parSequence
              .map(expr.withChildren(_))

          case _ =>
            expr.children
              .map(eval(_, buffer))
              .parSequence
              .map(expr.withChildren(_))
        }
      }

      eval(expression, 0)
    }

  def forExtent(expression: Expression)(implicit t: Timer[IO]): (Int, Extent) => IO[Expression] =
    (zoom: Int, extent: Extent) => {
      def eval(expr: Expression): IO[Expression] = {
        expr match {
          case unboundTileSource: UnboundTileSource =>
            unboundTileSource.resolveBindingForExtent(zoom, extent)
          case unboundSource: UnboundSource =>
            unboundSource.resolveBinding
          case boundSource: BoundSource =>
            IO.pure(boundSource)
          case _ =>
            expr.children
              .map(eval(_))
              .parSequence
              .map(expr.withChildren(_))
        }
      }

      eval(expression)
    }
}

