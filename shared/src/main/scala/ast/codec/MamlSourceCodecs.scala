package com.azavea.maml.ast.codec

import com.azavea.maml.ast._

import io.circe._
import io.circe.syntax._
import io.circe.generic.extras.semiauto.{
  deriveDecoder => genericallyDeriveDecoder,
  deriveEncoder => genericallyDeriveEncoder
}
import io.circe.generic.extras.Configuration


trait MamlSourceCodecs extends MamlUtilityCodecs {
  implicit def conf: Configuration
  implicit def mamlDecoder: Decoder[Expression]
  implicit def mamlEncoder: Encoder[Expression]

  implicit lazy val decodeIntLiteral: Decoder[IntLit] = genericallyDeriveDecoder
  implicit lazy val encodeIntLiteral: Encoder[IntLit] = genericallyDeriveEncoder

  implicit lazy val decodeIntVariable: Decoder[IntVar] = genericallyDeriveDecoder
  implicit lazy val encodeIntVariable: Encoder[IntVar] = genericallyDeriveEncoder

  implicit lazy val decodeDoubleLiteral: Decoder[DblLit] = genericallyDeriveDecoder
  implicit lazy val encodeDoubleLiteral: Encoder[DblLit] = genericallyDeriveEncoder

  implicit lazy val decodeDoubleVariable: Decoder[DblVar] = genericallyDeriveDecoder
  implicit lazy val encodeDoubleVariable: Encoder[DblVar] = genericallyDeriveEncoder

  implicit lazy val decodeBoolLiteral: Decoder[BoolLit] = genericallyDeriveDecoder
  implicit lazy val encodeBoolLiteral: Encoder[BoolLit] = genericallyDeriveEncoder

  implicit lazy val decodeBoolVariable: Decoder[BoolVar] = genericallyDeriveDecoder
  implicit lazy val encodeBoolVariable: Encoder[BoolVar] = genericallyDeriveEncoder

  implicit lazy val decodeGeomLiteral: Decoder[GeomLit] = genericallyDeriveDecoder
  implicit lazy val encodeGeomLiteral: Encoder[GeomLit] = genericallyDeriveEncoder

  implicit lazy val decodeGeomVariable: Decoder[GeomVar] = genericallyDeriveDecoder
  implicit lazy val encodeGeomVariable: Encoder[GeomVar] = genericallyDeriveEncoder

  implicit lazy val decodeRasterVariable: Decoder[RasterVar] = genericallyDeriveDecoder
  implicit lazy val encodeRasterVariable: Encoder[RasterVar] = genericallyDeriveEncoder
}

