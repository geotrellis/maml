package com.azavea.maml.ast.codec.tree

import com.azavea.maml.ast._
import com.azavea.maml.ast.codec._

import io.circe._
import io.circe.syntax._
import io.circe.generic.extras.semiauto._
import io.circe.generic.extras.Configuration

import java.security.InvalidParameterException


trait ExpressionTreeCodec
    extends MamlOperationCodecs
       with MamlSourceCodecs
       with MamlUtilityCodecs {

  implicit def conf: Configuration =
    Configuration.default.withDiscriminator("type")

  /** TODO: Add codec paths besides `raster source` and `operation` when supported */
  implicit def mamlDecoder = Decoder.instance[Expression] { ma =>
    ma._type match {
      case Some("IntLit") => ma.as[IntLit]
      case Some("IntVar") => ma.as[IntVar]
      case Some("DblLit") => ma.as[DblLit]
      case Some("DblVar") => ma.as[DblVar]
      case Some("BoolLit") => ma.as[BoolLit]
      case Some("BoolVar") => ma.as[BoolVar]
      case Some("GeomLit") => ma.as[GeomLit]
      case Some("GeomVar") => ma.as[GeomVar]
      case Some("RasterLit") =>
        Left(DecodingFailure(s"Can't deserialize raster from JSON in: $ma", ma.history))
      case Some("RasterVar") => ma.as[RasterVar]
      case Some("Addition") => ma.as[Addition]
      case Some("Subtraction") => ma.as[Subtraction]
      case Some("Multiplication") => ma.as[Multiplication]
      case Some("Division") => ma.as[Division]
      case Some("Max") => ma.as[Max]
      case Some("Min") => ma.as[Min]
      case Some("Masking") => ma.as[Masking]
      case Some("Classification") => ma.as[Classification]
      case Some("FocalMax") => ma.as[FocalMax]
      case Some("FocalMin") => ma.as[FocalMin]
      case Some("FocalMean") => ma.as[FocalMean]
      case Some("FocalMedian") => ma.as[FocalMedian]
      case Some("FocalMode") => ma.as[FocalMode]
      case Some("FocalSum") => ma.as[FocalSum]
      case Some("FocalStdDev") => ma.as[FocalStdDev]
      case None =>
        Left(DecodingFailure(s"Unrecognized node: $ma", ma.history))
    }
  }

  implicit def mamlEncoder: Encoder[Expression] = new Encoder[Expression] {
    final def apply(ast: Expression): Json = ast match {
      case node: IntLit => node.asJson
      case node: IntVar => node.asJson
      case node: DblLit => node.asJson
      case node: DblVar => node.asJson
      case node: BoolLit => node.asJson
      case node: BoolVar => node.asJson
      case node: GeomLit => node.asJson
      case node: GeomVar => node.asJson
      case node: RasterLit[_] =>
        throw new InvalidParameterException(s"Can't serialize rasters to JSON in: $node")
      case node: RasterVar => node.asJson
      case node: Addition => node.asJson
      case node: Subtraction => node.asJson
      case node: Multiplication => node.asJson
      case node: Division => node.asJson
      case node: Max =>node.asJson
      case node: Min => node.asJson
      case node: Masking => node.asJson
      case node: Classification => node.asJson
      case node: FocalMax => node.asJson
      case node: FocalMin => node.asJson
      case node: FocalMean => node.asJson
      case node: FocalMedian => node.asJson
      case node: FocalMode => node.asJson
      case node: FocalSum => node.asJson
      case node: FocalStdDev => node.asJson
      case _ =>
        throw new InvalidParameterException(s"Unrecognized AST: $ast")
    }
  }
}

object ExpressionTreeCodec extends ExpressionTreeCodec with MamlOperationCodecs with MamlSourceCodecs
