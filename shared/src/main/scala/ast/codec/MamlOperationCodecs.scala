package com.azavea.maml.ast.codec

import com.azavea.maml.ast._
import com.azavea.maml.util._

import io.circe._
import io.circe.syntax._
import io.circe.generic.extras.semiauto._
import io.circe.generic.extras.Configuration

import java.security.InvalidParameterException


trait MamlOperationCodecs extends MamlUtilityCodecs {
  implicit def conf: Configuration
  implicit def mamlDecoder: Decoder[Expression]
  implicit def mamlEncoder: Encoder[Expression]

  implicit lazy val decodeAddition: Decoder[Addition] = deriveDecoder
  implicit lazy val encodeAddition: Encoder[Addition] = deriveEncoder

  implicit lazy val decodeSubtraction: Decoder[Subtraction] = deriveDecoder
  implicit lazy val encodeSubtraction: Encoder[Subtraction] = deriveEncoder

  implicit lazy val decodeMultiplication: Decoder[Multiplication] = deriveDecoder
  implicit lazy val encodeMultiplication: Encoder[Multiplication] = deriveEncoder

  implicit lazy val decodeDivision: Decoder[Division] = deriveDecoder
  implicit lazy val encodeDivision: Encoder[Division] = deriveEncoder

  implicit lazy val decodeMax: Decoder[Max] = deriveDecoder
  implicit lazy val encodeMax: Encoder[Max] = deriveEncoder

  implicit lazy val decodeMin: Decoder[Min] = deriveDecoder
  implicit lazy val encodeMin: Encoder[Min] = deriveEncoder

  implicit lazy val decodeMasking: Decoder[Masking] = deriveDecoder
  implicit lazy val encodeMasking: Encoder[Masking] = deriveEncoder

  implicit lazy val decodeClassification: Decoder[Classification] = deriveDecoder
  implicit lazy val encodeClassification: Encoder[Classification] = deriveEncoder

  implicit lazy val decodeFocalMax: Decoder[FocalMax] = deriveDecoder
  implicit lazy val encodeFocalMax: Encoder[FocalMax] = deriveEncoder

  implicit lazy val decodeFocalMin: Decoder[FocalMin] = deriveDecoder
  implicit lazy val encodeFocalMin: Encoder[FocalMin] = deriveEncoder

  implicit lazy val decodeFocalMean: Decoder[FocalMean] = deriveDecoder
  implicit lazy val encodeFocalMean: Encoder[FocalMean] = deriveEncoder

  implicit lazy val decodeFocalMedian: Decoder[FocalMedian] = deriveDecoder
  implicit lazy val encodeFocalMedian: Encoder[FocalMedian] = deriveEncoder

  implicit lazy val decodeFocalMode: Decoder[FocalMode] = deriveDecoder
  implicit lazy val encodeFocalMode: Encoder[FocalMode] = deriveEncoder

  implicit lazy val decodeFocalSum: Decoder[FocalSum] = deriveDecoder
  implicit lazy val encodeFocalSum: Encoder[FocalSum] = deriveEncoder

  implicit lazy val decodeFocalStdDev: Decoder[FocalStdDev] = deriveDecoder
  implicit lazy val encodeFocalStdDev: Encoder[FocalStdDev] = deriveEncoder

  implicit lazy val decodeGreater: Decoder[Greater] = deriveDecoder
  implicit lazy val encodeGreater: Encoder[Greater] = deriveEncoder

  implicit lazy val decodeGreaterOrEqual: Decoder[GreaterOrEqual] = deriveDecoder
  implicit lazy val encodeGreaterOrEqual: Encoder[GreaterOrEqual] = deriveEncoder

  implicit lazy val decodeEqual: Decoder[Equal] = deriveDecoder
  implicit lazy val encodeEqual: Encoder[Equal] = deriveEncoder

  implicit lazy val decodeLessOrEqual: Decoder[LessOrEqual] = deriveDecoder
  implicit lazy val encodeLessOrEqual: Encoder[LessOrEqual] = deriveEncoder

  implicit lazy val decodeLess: Decoder[Less] = deriveDecoder
  implicit lazy val encodeLess: Encoder[Less] = deriveEncoder
}
