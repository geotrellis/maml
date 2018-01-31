package com.azavea.maml.ast

import io.circe._
import io.circe.optics.JsonPath._
import io.circe.generic.extras.Configuration
import cats.data._

package object codec extends MamlUtilityCodecs {

  implicit class CirceMapAlgebraJsonMethods(val self: Json) {
    def _type: Option[String] = root._type.string.getOption(self)
    def _label: Option[String] = root.metadata.label.string.getOption(self)
    def _symbol: Option[String] = root.selectDynamic("apply").string.getOption(self)

    def _keys: Option[Seq[String]] = root.obj.getOption(self).map(_.keys.toSeq)
  }

  implicit class CirceMapAlgebraHCursorMethods(val self: HCursor) {
    def _type: Option[String] = self.value._type
    def _label: Option[String] = self.value._label
    def _symbol: Option[String] = self.value._symbol

    def _keys: Option[Seq[String]] = self.value._keys
  }
}
