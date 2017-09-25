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

  implicit lazy val decodeTileSource: Decoder[TileSource] = deriveDecoder
  implicit lazy val encodeTileSource: Encoder[TileSource] = deriveEncoder

  implicit lazy val decodeIntSrc: Decoder[IntSource] = deriveDecoder
  implicit lazy val encodeIntSrc: Encoder[IntSource] = deriveEncoder

  implicit lazy val decodeDoubleSrc: Decoder[DoubleSource] = deriveDecoder
  implicit lazy val encodeDoubleSrc: Encoder[DoubleSource] = deriveEncoder

  implicit lazy val decodeBoolSrc: Decoder[BoolSource] = deriveDecoder
  implicit lazy val encodeBoolSrc: Encoder[BoolSource] = deriveEncoder

  implicit lazy val decodeGeomSrc: Decoder[GeomSource] = deriveDecoder
  implicit lazy val encodeGeomSrc: Encoder[GeomSource] = deriveEncoder

  implicit lazy val decodeIntLiteral: Decoder[IntLiteral] = deriveDecoder
  implicit lazy val encodeIntLiteral: Encoder[IntLiteral] = deriveEncoder

  implicit lazy val decodeDoubleLiteral: Decoder[DoubleLiteral] = deriveDecoder
  implicit lazy val encodeDoubleLiteral: Encoder[DoubleLiteral] = deriveEncoder

  implicit lazy val decodeBoolLiteral: Decoder[BoolLiteral] = deriveDecoder
  implicit lazy val encodeBoolLiteral: Encoder[BoolLiteral] = deriveEncoder
}

