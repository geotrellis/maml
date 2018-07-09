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

  implicit lazy val decodeIntLiteral: Decoder[IntLit] = deriveDecoder
  implicit lazy val encodeIntLiteral: Encoder[IntLit] = deriveEncoder

  implicit lazy val decodeIntVariable: Decoder[IntVar] = deriveDecoder
  implicit lazy val encodeIntVariable: Encoder[IntVar] = deriveEncoder

  implicit lazy val decodeDoubleLiteral: Decoder[DblLit] = deriveDecoder
  implicit lazy val encodeDoubleLiteral: Encoder[DblLit] = deriveEncoder

  implicit lazy val decodeDoubleVariable: Decoder[DblVar] = deriveDecoder
  implicit lazy val encodeDoubleVariable: Encoder[DblVar] = deriveEncoder

  implicit lazy val decodeBoolLiteral: Decoder[BoolLit] = deriveDecoder
  implicit lazy val encodeBoolLiteral: Encoder[BoolLit] = deriveEncoder

  implicit lazy val decodeBoolVariable: Decoder[BoolVar] = deriveDecoder
  implicit lazy val encodeBoolVariable: Encoder[BoolVar] = deriveEncoder

  implicit lazy val decodeGeomLiteral: Decoder[GeomLit] = deriveDecoder
  implicit lazy val encodeGeomLiteral: Encoder[GeomLit] = deriveEncoder

  implicit lazy val decodeGeomVariable: Decoder[GeomVar] = deriveDecoder
  implicit lazy val encodeGeomVariable: Encoder[GeomVar] = deriveEncoder

  implicit lazy val decodeRasterVariable: Decoder[RasterVar] = deriveDecoder
  implicit lazy val encodeRasterVariable: Encoder[RasterVar] = deriveEncoder
}

