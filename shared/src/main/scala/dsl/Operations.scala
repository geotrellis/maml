package maml.dsl

import maml.ast._


trait Operations {
  implicit class LocalExpressionMethods[Exp <: Expression](e: Exp) {
    def +(other: Expression) = Addition(List(e, other))
    def -(other: Expression) = Subtraction(List(e, other))
    def *(other: Expression) = Multiplication(List(e, other))
    def /(other: Expression) = Division(List(e, other))
    def <(other: Expression) = Less(List(e, other))
    def <=(other: Expression) = LessOrEqual(List(e, other))
    def ====(other: Expression) = Equal(List(e, other))
    def >=(other: Expression) = GreaterOrEqual(List(e, other))
    def >(other: Expression) = Greater(List(e, other))
  }
}
