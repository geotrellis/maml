package maml.ast.utility

import maml.ast.codec.MamlUtilityCodecs._

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
  // How exposed should this be to the api?
  val options: ClassMap.Options = ClassMap.Options()

  //lazy val mapStrategy =
  //  new MapStrategy(options.boundaryType, options.ndValue, options.fallback, false)

  //def toBreakMap =
  //  new BreakMap(classifications, mapStrategy, { i: Double => isNoData(i) })

  //def toColorMap =
  //  ColorMap(
  //    classifications,
  //    ColorMap.Options(
  //      options.boundaryType,
  //      options.ndValue,
  //      options.fallback
  //    )
  //  )
}

object ClassMap {
  @JsonCodec
  case class Options(
    boundaryType: ClassBoundaryType = LessThanOrEqualTo,
    ndValue: Int = Int.MinValue,
    fallback: Int = Int.MinValue
  )
}
