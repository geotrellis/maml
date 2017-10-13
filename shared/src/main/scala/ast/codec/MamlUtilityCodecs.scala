package com.azavea.maml.ast.codec

import com.azavea.maml.ast._
import com.azavea.maml.util._

import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.optics.JsonPath._
import cats.syntax.functor._

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
    Decoder[String].emap {
      case "lessThan" => Right(LessThan)
      case "lessThanOrEqualTo" => Right(LessThanOrEqualTo)
      case "exact" => Right(Exact)
      case "greaterThanOrEqualTo" => Right(GreaterThanOrEqualTo)
      case "greaterThan" => Right(GreaterThan)
      case unrecognized => Left(s"Unable to parse $unrecognized as ClassBoundaryType")
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

  // This won't actually work - NESW neighborhoods will *always* succeed in decoding to Square
  implicit val neighborhoodDecoder: Decoder[Neighborhood] = Decoder.instance[Neighborhood] { n =>
    n._type match {
      case Some("square") => n.as[Square]
      case Some("circle") => n.as[Circle]
      case Some("nesw") => n.as[Nesw]
      case Some("wedge") => n.as[Wedge]
      case Some("annulus") => n.as[Annulus]
    }
  }
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

  implicit val mamlKindDecoder: Decoder[MamlKind] = Decoder[String].emap({
    case "tile" => Right(MamlKind.Tile)
    case "int" => Right(MamlKind.Scalar)
    case "double" => Right(MamlKind.Scalar)
    case "geom" => Right(MamlKind.Geom)
    case "bool" => Right(MamlKind.Bool)
    case unrecognized => Left(s"Unrecognized MamlKind: $unrecognized")
  })
  implicit val mamlKindEncoder: Encoder[MamlKind] =
    Encoder.encodeString.contramap[MamlKind]({ mk =>
      mk match {
        case MamlKind.Tile => "tile"
        case MamlKind.Scalar => "int"
        case MamlKind.Scalar => "double"
        case MamlKind.Geom => "geom"
        case MamlKind.Bool => "bool"
        case unrecognized =>
          throw new InvalidParameterException(s"Unrecognized mamlKind: $unrecognized")
      }
    })


  implicit val squareNeighborhoodDecoder: Decoder[Square] =
    Decoder.forProduct1("extent")(Square.apply)
  implicit val squareNeighborhoodEncoder: Encoder[Square] =
    Encoder.forProduct2("extent", "type")(op => (op.extent, "square"))

  implicit val circleNeighborhoodDecoder: Decoder[Circle] =
    Decoder.forProduct1("radius")(Circle.apply)
  implicit val circleNeighborhoodEncoder: Encoder[Circle] =
    Encoder.forProduct2("radius", "type")(op => (op.radius, "circle"))

  implicit val neswNeighborhoodDecoder: Decoder[Nesw] =
    Decoder.forProduct1("extent")(Nesw.apply)
  implicit val neswNeighborhoodEncoder: Encoder[Nesw] =
    Encoder.forProduct2("extent", "type")(op => (op.extent, "nesw"))

  implicit val wedgeNeighborhoodDecoder: Decoder[Wedge] =
    Decoder.forProduct3("radius", "startAngle", "endAngle")(Wedge.apply)
  implicit val wedgeNeighborhoodEncoder: Encoder[Wedge] =
    Encoder.forProduct4("radius", "startAngle", "endAngle", "type")(op => (op.radius, op.startAngle, op.endAngle, "wedge"))

  implicit val annulusNeighborhoodDecoder: Decoder[Annulus] =
    Decoder.forProduct2("innerRadius", "outerRadius")(Annulus.apply)
  implicit val annulusNeighborhoodEncoder: Encoder[Annulus] =
    Encoder.forProduct3("innerRadius", "outerRadius", "type")(op => (op.innerRadius, op.outerRadius, "annulus"))
}

object MamlUtilityCodecs extends MamlUtilityCodecs
