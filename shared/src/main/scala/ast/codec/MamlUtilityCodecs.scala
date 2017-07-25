package maml.ast.codec

import maml.ast._
import maml.ast.utility._

import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.optics.JsonPath._

import java.security.InvalidParameterException
import java.util.UUID
import scala.util.Try


trait MamlUtilityCodecs {
  implicit val decodeKeyDouble: KeyDecoder[Double] = new KeyDecoder[Double] {
    final def apply(key: String): Option[Double] = Try(key.toDouble).toOption
  }
  implicit val encodeKeyDouble: KeyEncoder[Double] = new KeyEncoder[Double] {
    final def apply(key: Double): String = key.toString
  }

  implicit val decodeKeyUUID: KeyDecoder[UUID] = new KeyDecoder[UUID] {
    final def apply(key: String): Option[UUID] = Try(UUID.fromString(key)).toOption
  }
  implicit val encodeKeyUUID: KeyEncoder[UUID] = new KeyEncoder[UUID] {
    final def apply(key: UUID): String = key.toString
  }

  implicit lazy val classBoundaryDecoder: Decoder[ClassBoundaryType] =
    Decoder[String].map {
      case "lessThan" => LessThan
      case "lessThanOrEqualTo" => LessThanOrEqualTo
      case "exact" => Exact
      case "greaterThanOrEqualTo" => GreaterThanOrEqualTo
      case "greaterThan" => GreaterThan
      case unrecognized =>
        throw new InvalidParameterException(s"'$unrecognized' is not a recognized ClassBoundaryType")
    }

  implicit lazy val classBoundaryEncoder: Encoder[ClassBoundaryType] =
    Encoder.encodeString.contramap[ClassBoundaryType]({ cbType =>
      cbType match {
        case LessThan => "lessThan"
        case LessThanOrEqualTo => "lessThanOrEqualTo"
        case Exact => "exact"
        case GreaterThanOrEqualTo => "greaterThanOrEqualTo"
        case GreaterThan => "greaterThan"
        case unrecognized =>
          throw new InvalidParameterException(s"'$unrecognized' is not a recognized ClassBoundaryType")
      }
    })

  implicit val colorRampDecoder: Decoder[ColorRamp] =
    Decoder[Vector[Int]].map({ vec => ColorRamp(vec) })

  implicit val colorRampEncoder: Encoder[ColorRamp] = new Encoder[ColorRamp] {
    final def apply(cRamp: ColorRamp): Json = cRamp.colors.toArray.asJson
  }

  implicit val histogramDecoder = Decoder.instance[Histogram]({ curs =>
    curs.get[Map[Double, Int]]("counts") match {
      case Right(counts) => Right(Histogram(counts))
      case Left(err) => Left(DecodingFailure(s"Unable to parse histogram", curs.history))
    }
  })

  implicit val histogramEncoder: Encoder[Histogram] =
    Encoder.forProduct1("counts")(hist => (hist.counts))

  implicit val neighborhoodDecoder: Decoder[Neighborhood] = Decoder.instance[Neighborhood] { n =>
    n.kind match {
      case Some("square") => n.as[Square]
      case Some("circle") => n.as[Circle]
      case Some("nesw") => n.as[Nesw]
      case Some("wedge") => n.as[Wedge]
      case Some("annulus") => n.as[Annulus]
      case unrecognized => Left(DecodingFailure(s"Unrecognized neighborhood: $unrecognized", n.history))
    }
  }

  implicit val mamlKindEncoder: Encoder[MamlKind] =
    Encoder.encodeString.contramap[MamlKind]({ mk =>
      mk match {
        case MamlKind.Tile => "tile"
        case MamlKind.Scalar => "scalar"
        case unrecognized =>
          throw new InvalidParameterException(s"Unrecognized mamlKind: $unrecognized")
      }
    })

  implicit val mamlKindTileEncoder: Encoder[MamlKind.Tile.type] =
    Encoder.encodeString.contramap[MamlKind.Tile.type]({ _ => "tile" })
  implicit val mamlKindScalarEncoder: Encoder[MamlKind.Scalar.type] =
    Encoder.encodeString.contramap[MamlKind.Scalar.type]({ _ => "scalar" })

  implicit val mamlKindDecoder: Decoder[MamlKind] = Decoder[String].map({
    case "tile" => MamlKind.Tile
    case "scalar" => MamlKind.Scalar
  })

  implicit val neighborhoodEncoder: Encoder[Neighborhood] = new Encoder[Neighborhood] {
    final def apply(n: Neighborhood): Json = n match {
      case square: Square => square.asJson
      case circle: Circle => circle.asJson
      case nesw: Nesw => nesw.asJson
      case wedge: Wedge => wedge.asJson
      case annulus: Annulus => annulus.asJson
      case unrecognized =>
        throw new InvalidParameterException(s"Unrecognized neighborhood: $unrecognized")
    }
  }

  implicit val squareNeighborhoodDecoder: Decoder[Square] =
    Decoder.forProduct1("extent")(Square.apply)
  implicit val squareNeighborhoodEncoder: Encoder[Square] =
    Encoder.forProduct2("extent", "kind")(op => (op.extent, "square"))

  implicit val circleNeighborhoodDecoder: Decoder[Circle] =
    Decoder.forProduct1("radius")(Circle.apply)
  implicit val circleNeighborhoodEncoder: Encoder[Circle] =
    Encoder.forProduct2("radius", "kind")(op => (op.radius, "circle"))

  implicit val neswNeighborhoodDecoder: Decoder[Nesw] =
    Decoder.forProduct1("extent")(Nesw.apply)
  implicit val neswNeighborhoodEncoder: Encoder[Nesw] =
    Encoder.forProduct2("extent", "kind")(op => (op.extent, "nesw"))

  implicit val wedgeNeighborhoodDecoder: Decoder[Wedge] =
    Decoder.forProduct3("radius", "startAngle", "endAngle")(Wedge.apply)
  implicit val wedgeNeighborhoodEncoder: Encoder[Wedge] =
    Encoder.forProduct4("radius", "startAngle", "endAngle", "kind")(op => (op.radius, op.startAngle, op.endAngle, "wedge"))

  implicit val annulusNeighborhoodDecoder: Decoder[Annulus] =
    Decoder.forProduct2("innerRadius", "outerRadius")(Annulus.apply)
  implicit val annulusNeighborhoodEncoder: Encoder[Annulus] =
    Encoder.forProduct3("innerRadius", "outerRadius", "kind")(op => (op.innerRadius, op.outerRadius, "annulus"))
}

object MamlUtilityCodecs extends MamlUtilityCodecs
