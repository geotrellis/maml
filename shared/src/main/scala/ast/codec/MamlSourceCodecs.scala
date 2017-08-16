package maml.ast.codec

import maml.ast._

import io.circe._
import io.circe.syntax._
import io.circe.generic.extras.semiauto._
import io.circe.generic.extras.Configuration

import java.security.InvalidParameterException


trait MamlSourceCodecs extends MamlUtilityCodecs {
  implicit def conf: Configuration
  implicit def mamlDecoder: Decoder[Expression]
  implicit def mamlEncoder: Encoder[Expression]

  implicit lazy val decodeSource: Decoder[TileSource.type] = deriveDecoder
  implicit lazy val encodeSource: Encoder[TileSource.type] = deriveEncoder

  implicit lazy val decodeInt: Decoder[IntSource] = deriveDecoder
  implicit lazy val encodeInt: Encoder[IntSource] = deriveEncoder

  implicit lazy val decodeDouble: Decoder[DoubleSource] = deriveDecoder
  implicit lazy val encodeDouble: Encoder[DoubleSource] = deriveEncoder
}

