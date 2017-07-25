package maml

import maml.ast.utility._
import maml.error.InterpreterError

import io.circe._
import io.circe.optics.JsonPath._
import cats.data._
import Validated._


package object ast {
  type Interpreted[A] = ValidatedNel[InterpreterError, A]

  implicit class CirceMapAlgebraJsonMethods(val self: Json) {
    //def _id: Option[UUID] = root.id.string.getOption(self).map(UUID.fromString(_))
    def kind: Option[String] = root.kind.string.getOption(self)
    def _label: Option[String] = root.metadata.label.string.getOption(self)
    def _symbol: Option[String] = root.selectDynamic("apply").string.getOption(self)

    def _fields: Option[Seq[String]] = root.obj.getOption(self).map(_.fields)
  }

  implicit class CirceMapAlgebraHCursorMethods(val self: HCursor) {
    //def _id: Option[UUID] = self.value._id
    def kind: Option[String] = self.value.kind
    def _label: Option[String] = self.value._label
    def _symbol: Option[String] = self.value._symbol

    def _fields: Option[Seq[String]] = self.value._fields
  }

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
