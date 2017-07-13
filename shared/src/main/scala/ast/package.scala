package maml

import maml.ast.codec._
import maml.ast.operation._
import maml.ast.operation.unary._
import maml.ast.operation.binary._
import maml.ast.utility._
import maml.ast.metadata._

import io.circe._
import io.circe.optics.JsonPath._

import java.util.UUID


package object ast {

  implicit class CirceMapAlgebraJsonMethods(val self: Json) {
    def _id: Option[UUID] = root.id.string.getOption(self).map(UUID.fromString(_))
    def _type: Option[String] = root.`type`.string.getOption(self)
    def _label: Option[String] = root.metadata.label.string.getOption(self)
    def _symbol: Option[String] = root.selectDynamic("apply").string.getOption(self)

    def _fields: Option[Seq[String]] = root.obj.getOption(self).map(_.fields)
  }

  implicit class CirceMapAlgebraHCursorMethods(val self: HCursor) {
    def _id: Option[UUID] = self.value._id
    def _type: Option[String] = self.value._type
    def _label: Option[String] = self.value._label
    def _symbol: Option[String] = self.value._symbol

    def _fields: Option[Seq[String]] = self.value._fields
  }

  implicit class MamlTreeHelperMethods(val self: MamlTree) {
    private def generateMetadata = Some(NodeMetadata(
      Some(s"${self.metadata.flatMap(_.label).getOrElse(self.id)}"),
      None,
      None
    ))

    def classify(classmap: ClassMap) =
      Classification(List(self), UUID.randomUUID(), generateMetadata, classmap)

    def +(other: MamlTree): Operation =
      Addition(List(self, other), UUID.randomUUID(), generateMetadata)

    def -(other: MamlTree): Operation =
      Subtraction(List(self, other), UUID.randomUUID(), generateMetadata)

    def *(other: MamlTree): Operation =
      Multiplication(List(self, other), UUID.randomUUID(), generateMetadata)

    def /(other: MamlTree): Operation =
      Division(List(self, other), UUID.randomUUID(), generateMetadata)

    def max(other: MamlTree): Operation =
      Max(List(self, other), UUID.randomUUID(), generateMetadata)

    def min(other: MamlTree): Operation =
      Min(List(self, other), UUID.randomUUID(), generateMetadata)
  }
}
