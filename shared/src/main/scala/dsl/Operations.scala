package maml.dsl

import maml.ast._

import cats._

// --- //

trait Operations {
  implicit class LocalExpressionMethods[T: Monoid, Exp <: Expression[T]](e: Exp) {
    def +(other: Expression[T]) = Addition(List(e, other), Monoid.empty)
    def -(other: Expression[T]) = Subtraction(List(e, other), Monoid.empty)
    def *(other: Expression[T]) = Multiplication(List(e, other), Monoid.empty)
    def /(other: Expression[T]) = Division(List(e, other), Monoid.empty)
    def <(other: Expression[T]) = Less(List(e, other), Monoid.empty)
    def <=(other: Expression[T]) = LessOrEqual(List(e, other), Monoid.empty)
    def ====(other: Expression[T]) = Equal(List(e, other), Monoid.empty)
    def >=(other: Expression[T]) = GreaterOrEqual(List(e, other), Monoid.empty)
    def >(other: Expression[T]) = Greater(List(e, other), Monoid.empty)
  }
}
