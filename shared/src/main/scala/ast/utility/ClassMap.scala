package maml.ast.utility

import maml.ast._

import io.circe._
import io.circe.syntax._
import io.circe.generic.JsonCodec

import java.security.InvalidParameterException

// --- //

trait ClassBoundaryType
case object LessThan extends ClassBoundaryType
case object LessThanOrEqualTo extends ClassBoundaryType
case object Exact extends ClassBoundaryType
case object GreaterThanOrEqualTo extends ClassBoundaryType
case object GreaterThan extends ClassBoundaryType

object ClassBoundaryType {
  implicit lazy val classBoundaryDecoder: Decoder[ClassBoundaryType] =
    Decoder[String].emap {
      case "lessThan" => Right(LessThan)
      case "lessThanOrEqualTo" => Right(LessThanOrEqualTo)
      case "exact" => Right(Exact)
      case "greaterThanOrEqualTo" => Right(GreaterThanOrEqualTo)
      case "greaterThan" => Right(GreaterThan)
      case unrecognized => Left(s"Unable to parse $unrecognized as ClassBoundaryType")
    }

  implicit lazy val classBoundaryEncoder: Encoder[ClassBoundaryType] =
    Encoder.encodeString.contramap[ClassBoundaryType] { cbType =>
      cbType match {
        case LessThan => "lessThan"
        case LessThanOrEqualTo => "lessThanOrEqualTo"
        case Exact => "exact"
        case GreaterThanOrEqualTo => "greaterThanOrEqualTo"
        case GreaterThan => "greaterThan"
        case unrecognized =>
          throw new InvalidParameterException(s"'$unrecognized' is not a recognized ClassBoundaryType")
      }
    }
}

@JsonCodec
case class ClassMap(classifications: Map[Double, Int]) {
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
