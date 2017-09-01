package maml.ast

import io.circe._
import io.circe.syntax._
import io.circe.generic.extras.semiauto._
import io.circe.generic.extras.Configuration

import java.security.InvalidParameterException


case class ValueReaderTileSource[T](bucket: String, root: String, layerId: String, extra: T) extends Source[T] {
  val kind = MamlKind.Tile
  def id = s"ValueReaderTileSrc-$bucket-$root-$layerId"
}


object ValueReaderTileSource {
  implicit def conf: Configuration =
    Configuration.default.withDefaults.withDiscriminator("type")

  implicit def decodeValueReaderTileSource[T: Decoder]: Decoder[ValueReaderTileSource[T]] = deriveDecoder
  implicit def encodeValueReaderTileSource[T: Encoder]: Encoder[ValueReaderTileSource[T]] = deriveEncoder
}
