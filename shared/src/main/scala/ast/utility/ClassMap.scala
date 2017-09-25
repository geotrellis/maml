package com.azavea.maml.ast.utility

import com.azavea.maml.ast.codec.MamlUtilityCodecs._

import io.circe._
import io.circe.syntax._
import io.circe.generic.JsonCodec


trait ClassBoundaryType
case object LessThan extends ClassBoundaryType
case object LessThanOrEqualTo extends ClassBoundaryType
case object Exact extends ClassBoundaryType
case object GreaterThanOrEqualTo extends ClassBoundaryType
case object GreaterThan extends ClassBoundaryType


@JsonCodec
case class ClassMap(
  classifications: Map[Double, Int]
) {
  val options: ClassMap.Options = ClassMap.Options()
}

object ClassMap {
  @JsonCodec
  case class Options(
    boundaryType: ClassBoundaryType = LessThanOrEqualTo,
    ndValue: Int = Int.MinValue,
    fallback: Int = Int.MinValue
  )
}
