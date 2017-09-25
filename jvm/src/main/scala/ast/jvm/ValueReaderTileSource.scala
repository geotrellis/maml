package com.azavea.maml.ast

import io.circe._
import io.circe.syntax._
import io.circe.generic.extras.semiauto._
import io.circe.generic.extras.Configuration

import java.security.InvalidParameterException


case class ValueReaderTileSource(bucket: String, root: String, layerId: String) extends Source {
  val kind = MamlKind.Tile
  def id = s"ValueReaderTileSrc-$bucket-$root-$layerId"
}


object ValueReaderTileSource {
  implicit def conf: Configuration =
    Configuration.default.withDefaults.withDiscriminator("type")

  implicit lazy val decodeValueReaderTileSource: Decoder[ValueReaderTileSource] = deriveDecoder
  implicit lazy val encodeValueReaderTileSource: Encoder[ValueReaderTileSource] = deriveEncoder
}

