package com.azavea.maml.sr

import cats.Functor
import higherkindness.droste.data.Fix
import higherkindness.droste.{Algebra, Coalgebra}

sealed trait ExpressionF[A]

object ExpressionF {
  type Exp = Fix[ExpressionF]

  case class Addition[A](l: A, r: A) extends ExpressionF[A]
  case class Substraction[A](l: A, r: A) extends ExpressionF[A]

  // def add[A](l: A, r: A) = Addition(l, r)

  implicit val expressionFFunctor: Functor[ExpressionF] =
    new Functor[ExpressionF] {
      def map[A, B](fa: ExpressionF[A])(f: A => B): ExpressionF[B] = {
        fa match {
          case Addition(l, r)     => Addition(f(l), f(r))
          case Substraction(l, r) => Substraction(f(l), f(r))
        }
      }
    }


}
