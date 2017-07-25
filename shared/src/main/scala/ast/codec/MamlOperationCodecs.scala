package maml.ast.codec

import maml.ast._
import maml.ast.utility._
import maml.ast.codec._

import io.circe._
import io.circe.syntax._

import java.security.InvalidParameterException


trait MamlOperationCodecs extends MamlUtilityCodecs {
  implicit def mamlDecoder: Decoder[Expression]
  implicit def mamlEncoder: Encoder[Expression]

  // Codec routing for Operations
  implicit lazy val decodeOperations = Decoder.instance[Operation] { ma =>
    ma._symbol match {
      case Some("+") => ma.as[Addition]
      case Some("-") => ma.as[Subtraction]
      case Some("/") => ma.as[Division]
      case Some("*") => ma.as[Multiplication]
      case Some("mask") => ma.as[Masking]
      case Some("min") => ma.as[Min]
      case Some("max") => ma.as[Max]
      case Some("classify") => ma.as[Classification]
      case Some("focalMax") => ma.as[FocalMax]
      case Some("focalMin") => ma.as[FocalMin]
      case Some("focalMean") => ma.as[FocalMean]
      case Some("focalMedian") => ma.as[FocalMedian]
      case Some("focalMode") => ma.as[FocalMode]
      case Some("focalSum") => ma.as[FocalSum]
      case Some("focalStdDev") => ma.as[FocalStdDev]
      case Some(unrecognized) =>
        Left(DecodingFailure(s"Unrecognized node type: $unrecognized", ma.history))
      case None =>
        Left(DecodingFailure("The property 'apply' is mandatory on all AST operations", ma.history))
    }
  }

  implicit lazy val encodeOperations: Encoder[Operation] = new Encoder[Operation] {
    final def apply(op: Operation): Json = op match {
      case addition: Addition =>
        addition.asJson
      case subtraction: Subtraction =>
        subtraction.asJson
      case division: Division =>
        division.asJson
      case multiplication: Multiplication =>
        multiplication.asJson
      case masking: Masking =>
        masking.asJson
      case min: Min =>
        min.asJson
      case max: Max =>
        max.asJson
      case classification: Classification =>
        classification.asJson
      case fMax: FocalMax =>
        fMax.asJson
      case fMin: FocalMin =>
        fMin.asJson
      case fMean: FocalMean =>
        fMean.asJson
      case fMedian: FocalMedian =>
        fMedian.asJson
      case fMode: FocalMode =>
        fMode.asJson
      case fSum: FocalSum =>
        fSum.asJson
      case fStdDev: FocalStdDev =>
        fStdDev.asJson
      case operation =>
        throw new InvalidParameterException(s"Encoder for $operation not yet implemented")
    }
  }

  /** NOTE: We need to keep these specialized encoder/decoders around for correct parsing of trees */
  implicit lazy val decodeAddition: Decoder[Addition] =
    Decoder.forProduct1("children")(Addition.apply)
  implicit lazy val encodeAddition: Encoder[Addition] =
    Encoder.forProduct2("apply", "children")(op => (op.symbol, op.children))

  implicit lazy val decodeSubtraction: Decoder[Subtraction] =
    Decoder.forProduct1("children")(Subtraction.apply)
  implicit lazy val encodeSubtraction: Encoder[Subtraction] =
    Encoder.forProduct2("apply", "children")(op => (op.symbol, op.children))

  implicit lazy val decodeDivision: Decoder[Division] =
    Decoder.forProduct1("children")(Division.apply)
  implicit lazy val encodeDivision: Encoder[Division] =
    Encoder.forProduct2("apply", "children")(op => (op.symbol, op.children))

  implicit lazy val decodeMultiplication: Decoder[Multiplication] =
    Decoder.forProduct1("children")(Multiplication.apply)
  implicit lazy val encodeMultiplication: Encoder[Multiplication] =
    Encoder.forProduct2("apply", "children")(op => (op.symbol, op.children))

  implicit lazy val decodeMax: Decoder[Max] =
    Decoder.forProduct1("children")(Max.apply)
  implicit lazy val encodeMax: Encoder[Max] =
    Encoder.forProduct2("apply", "children")(op => (op.symbol, op.children))

  implicit lazy val decodeMin: Decoder[Min] =
    Decoder.forProduct1("children")(Min.apply)
  implicit lazy val encodeMin: Encoder[Min] =
    Encoder.forProduct2("apply", "children")(op => (op.symbol, op.children))

  implicit lazy val decodeMasking: Decoder[Masking] =
    Decoder.forProduct2("children", "mask")(Masking.apply)
  implicit lazy val encodeMasking: Encoder[Masking] =
    Encoder.forProduct3("apply", "children", "mask")(op => (op.symbol, op.children, op.mask))

  implicit lazy val decodeClassification: Decoder[Classification] =
    Decoder.forProduct2("children", "classMap")(Classification.apply)
  implicit lazy val encodeClassification: Encoder[Classification] =
    Encoder.forProduct3("apply", "children", "classMap")(op => (op.symbol, op.children, op.classMap))

  implicit lazy val decodeFocalMax: Decoder[FocalMax] =
    Decoder.forProduct2("children", "neighborhood")(FocalMax.apply)
  implicit lazy val encodeFocalMax: Encoder[FocalMax] =
    Encoder.forProduct3("apply", "children", "neighborhood")(op => (op.symbol, op.children, op.neighborhood))

  implicit lazy val decodeFocalMin: Decoder[FocalMin] =
    Decoder.forProduct2("children", "neighborhood")(FocalMin.apply)
  implicit lazy val encodeFocalMin: Encoder[FocalMin] =
    Encoder.forProduct3("apply", "children", "neighborhood")(op => (op.symbol, op.children, op.neighborhood))

  implicit lazy val decodeFocalMean: Decoder[FocalMean] =
    Decoder.forProduct2("children", "neighborhood")(FocalMean.apply)
  implicit lazy val encodeFocalMean: Encoder[FocalMean] =
    Encoder.forProduct3("apply", "children", "neighborhood")(op => (op.symbol, op.children, op.neighborhood))

  implicit lazy val decodeFocalMedian: Decoder[FocalMedian] =
    Decoder.forProduct2("children", "neighborhood")(FocalMedian.apply)
  implicit lazy val encodeFocalMedian: Encoder[FocalMedian] =
    Encoder.forProduct3("apply", "children", "neighborhood")(op => (op.symbol, op.children, op.neighborhood))

  implicit lazy val decodeFocalMode: Decoder[FocalMode] =
    Decoder.forProduct2("children", "neighborhood")(FocalMode.apply)
  implicit lazy val encodeFocalMode: Encoder[FocalMode] =
    Encoder.forProduct3("apply", "children", "neighborhood")(op => (op.symbol, op.children, op.neighborhood))

  implicit lazy val decodeFocalSum: Decoder[FocalSum] =
    Decoder.forProduct2("children", "neighborhood")(FocalSum.apply)
  implicit lazy val encodeFocalSum: Encoder[FocalSum] =
    Encoder.forProduct3("apply", "children", "neighborhood")(op => (op.symbol, op.children, op.neighborhood))

  implicit lazy val decodeFocalStdDev: Decoder[FocalStdDev] =
    Decoder.forProduct2("children", "neighborhood")(FocalStdDev.apply)
  implicit lazy val encodeFocalStdDev: Encoder[FocalStdDev] =
    Encoder.forProduct3("apply", "children", "neighborhood")(op => (op.symbol, op.children, op.neighborhood))

}
