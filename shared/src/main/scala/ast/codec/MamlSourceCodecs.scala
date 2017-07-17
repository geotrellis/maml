package maml.ast.codec

import maml.ast._

import io.circe._
import io.circe.syntax._

import java.security.InvalidParameterException


trait MamlSourceCodecs {
  implicit def mamlDecoder: Decoder[MamlTree]
  implicit def mamlEncoder: Encoder[MamlTree]

  /** TODO: Add codec paths besides `raster source` and `operation` when supported */
  implicit val mamlSourceDecoder = Decoder.instance[Source] { ma =>
    ma._type match {
      case Some("tile") =>
        ma.as[TileSource]
      case Some("scalar") =>
        ma.as[ScalarSource]
      case Some(unrecognized) =>
        Left(DecodingFailure(s"Unrecognized Source node with type: $ma", ma.history))
      case None =>
        Left(DecodingFailure(s"Unrecognized Source node: $ma", ma.history))
    }
  }

  implicit val mamlSourceEncoder: Encoder[Source] = new Encoder[Source] {
    final def apply(ast: Source): Json = ast match {
      case src: TileSource =>
        src.asJson
      case const: ScalarSource =>
        const.asJson
      case _ =>
        throw new InvalidParameterException(s"Unrecognized AST: $ast")
    }
  }

  implicit lazy val decodeSource: Decoder[TileSource] =
    Decoder.forProduct2("id", "metadata")(TileSource.apply)
  implicit lazy val encodeSource: Encoder[TileSource] =
    Encoder.forProduct3("type", "id", "metadata")(op => (op.`type`, op.id, op.metadata))

  implicit lazy val decodeConstant: Decoder[ScalarSource] =
    Decoder.forProduct3("id", "scalar", "metadata")(ScalarSource.apply)
  implicit lazy val encodeConstant: Encoder[ScalarSource] =
    Encoder.forProduct4("type", "id", "scalar", "metadata")(op => (op.`type`, op.id, op.scalar, op.metadata))
}

