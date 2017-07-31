package maml

import maml.ast.utility._
import maml.error.InterpreterError

import io.circe._
import io.circe.optics.JsonPath._
import cats.data._
import Validated._


package object ast {

  implicit class MamlExpressionHelperMethods(val self: Expression) {
    def classify(classmap: ClassMap) =
      Classification(List(self), classmap)

    def +(other: Expression): Expression =
      Addition(List(self, other))

    def -(other: Expression): Expression =
      Subtraction(List(self, other))

    def *(other: Expression): Expression =
      Multiplication(List(self, other))

    def /(other: Expression): Expression =
      Division(List(self, other))

    def max(other: Expression): Expression =
      Max(List(self, other))

    def min(other: Expression): Expression =
      Min(List(self, other))
  }
}
