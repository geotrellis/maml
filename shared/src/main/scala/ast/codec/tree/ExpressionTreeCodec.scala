package maml.ast.codec.tree

import maml.ast._
import maml.ast.codec._

import io.circe._
import io.circe.syntax._
import io.circe.generic.extras.Configuration

import java.security.InvalidParameterException


trait ExpressionTreeCodec
    extends MamlOperationCodecs
       with MamlSourceCodecs
       with MamlUtilityCodecs {

  implicit def conf: Configuration =
    Configuration.default.withDefaults.withDiscriminator("type")

  /** TODO: Add codec paths besides `raster source` and `operation` when supported */
  implicit def mamlDecoder = Decoder.instance[Expression] { ma =>
    ma._type match {
      case Some("TileSource") => ma.as[TileSource]
      case Some("IntSource") => ma.as[IntSource]
      case Some("DoubleSource") => ma.as[DoubleSource]
      case Some("BoolSource") => ma.as[BoolSource]
      case Some("GeomSource") => ma.as[GeomSource]
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
      case None => Left(DecodingFailure(s"Unrecovnized leaf node: $ma", ma.history))
    }
  }

  implicit def mamlEncoder: Encoder[Expression] = new Encoder[Expression] {
    final def apply(ast: Expression): Json = ast match {
      case node: TileSource => node.asJson
      case node: IntSource => node.asJson
      case node: DoubleSource => node.asJson
      case node: BoolSource => node.asJson
      case node: GeomSource => node.asJson
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

object ExpressionTreeCodec extends ExpressionTreeCodec
