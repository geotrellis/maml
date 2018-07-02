package com.azavea.maml.ast.codec

import com.azavea.maml.ast._

import io.circe._
import io.circe.syntax._
import io.circe.generic.extras.semiauto._
import io.circe.generic.extras.Configuration

import java.security.InvalidParameterException


trait MamlSourceCodecs extends MamlUtilityCodecs {
  implicit def conf: Configuration
  implicit def mamlDecoder: Decoder[Expression]
  implicit def mamlEncoder: Encoder[Expression]

  implicit lazy val decodeIntLiteral: Decoder[IntLiteral] = deriveDecoder
  implicit lazy val encodeIntLiteral: Encoder[IntLiteral] = deriveEncoder

  implicit lazy val decodeDoubleLiteral: Decoder[DoubleLiteral] = deriveDecoder
  implicit lazy val encodeDoubleLiteral: Encoder[DoubleLiteral] = deriveEncoder

  implicit lazy val decodeBoolLiteral: Decoder[BoolLiteral] = deriveDecoder
  implicit lazy val encodeBoolLiteral: Encoder[BoolLiteral] = deriveEncoder
}

