package maml.ast.codec

import maml.ast._
import maml.ast.utility._
import maml.ast.codec._
import maml.ast.operation._
import maml.ast.operation.binary._
import maml.ast.operation.unary._

import io.circe._
import io.circe.syntax._

import java.security.InvalidParameterException


trait MamlOperationCodecs {
  implicit def mamlDecoder: Decoder[MamlTree]
  implicit def mamlEncoder: Encoder[MamlTree]

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
      case Some(unrecognized) =>
        Left(DecodingFailure(s"Unrecognized node type: $unrecognized", ma.history))
      case None =>
        Left(DecodingFailure("The property 'apply' is mandatory on all AST operations", ma.history))
    }
  }

  implicit lazy val encodeOperations: Encoder[Operation] = new Encoder[Operation] {
    final def apply(op: Operation): Json = op match {
      case addition: binary.Addition =>
        addition.asJson
      case subtraction: binary.Subtraction =>
        subtraction.asJson
      case division: binary.Division =>
        division.asJson
      case multiplication: binary.Multiplication =>
        multiplication.asJson
      case masking: binary.Masking =>
        masking.asJson
      case min: binary.Min =>
        min.asJson
      case max: binary.Max =>
        max.asJson
      case classification: unary.Classification =>
        classification.asJson
      case operation =>
        throw new InvalidParameterException(s"Encoder for $operation not yet implemented")
    }
  }

  /** NOTE: We need to keep these specialized encoder/decoders around for correct parsing of trees */
  implicit lazy val decodeAddition: Decoder[Addition] =
    Decoder.forProduct3("args", "id", "metadata")(Addition.apply)
  implicit lazy val encodeAddition: Encoder[Addition] =
    Encoder.forProduct4("apply", "args", "id", "metadata")(op => (op.symbol, op.args, op.id, op.metadata))

  implicit lazy val decodeSubtraction: Decoder[Subtraction] =
    Decoder.forProduct3("args", "id", "metadata")(Subtraction.apply)
  implicit lazy val encodeSubtraction: Encoder[Subtraction] =
    Encoder.forProduct4("apply", "args", "id", "metadata")(op => (op.symbol, op.args, op.id, op.metadata))

  implicit lazy val decodeDivision: Decoder[Division] =
    Decoder.forProduct3("args", "id", "metadata")(Division.apply)
  implicit lazy val encodeDivision: Encoder[Division] =
    Encoder.forProduct4("apply", "args", "id", "metadata")(op => (op.symbol, op.args, op.id, op.metadata))

  implicit lazy val decodeMultiplication: Decoder[Multiplication] =
    Decoder.forProduct3("args", "id", "metadata")(Multiplication.apply)
  implicit lazy val encodeMultiplication: Encoder[Multiplication] =
    Encoder.forProduct4("apply", "args", "id", "metadata")(op => (op.symbol, op.args, op.id, op.metadata))

  implicit lazy val decodeMasking: Decoder[Masking] =
    Decoder.forProduct4("args", "id", "metadata", "mask")(Masking.apply)
  implicit lazy val encodeMasking: Encoder[Masking] =
    Encoder.forProduct5("apply", "args", "id", "metadata", "mask")(op => (op.symbol, op.args, op.id, op.metadata, op.mask))

  implicit lazy val decodeClassification: Decoder[Classification] =
    Decoder.forProduct4("args", "id", "metadata", "classMap")(Classification.apply)
  implicit lazy val encodeClassification: Encoder[Classification] =
    Encoder.forProduct5("apply", "args", "id", "metadata", "classMap")(op => (op.symbol, op.args, op.id, op.metadata, op.classMap))

  implicit lazy val decodeMax: Decoder[Max] =
    Decoder.forProduct3("args", "id", "metadata")(Max.apply)
  implicit lazy val encodeMax: Encoder[Max] =
    Encoder.forProduct4("apply", "args", "id", "metadata")(op => (op.symbol, op.args, op.id, op.metadata))

  implicit lazy val decodeMin: Decoder[Min] =
    Decoder.forProduct3("args", "id", "metadata")(Min.apply)
  implicit lazy val encodeMin: Encoder[Min] =
    Encoder.forProduct4("apply", "args", "id", "metadata")(op => (op.symbol, op.args, op.id, op.metadata))
}
