package maml.ast.codec

import maml.ast._
import maml.ast.leaf._

import io.circe._
import io.circe.syntax._

import java.security.InvalidParameterException


trait MamlLeafCodecs {
  implicit def mamlDecoder: Decoder[MamlTree]
  implicit def mamlEncoder: Encoder[MamlTree]

  /** TODO: Add codec paths besides `raster source` and `operation` when supported */
  implicit val mamlLeafDecoder = Decoder.instance[Leaf] { ma =>
    ma._type match {
      case Some("tile") =>
        ma.as[TileSource]
      case Some("scalar") =>
        ma.as[ScalarSource]
      case _ =>
        Left(DecodingFailure(s"Unrecognized leaf node: $ma", ma.history))
    }
  }

  implicit val mamlLeafEncoder: Encoder[Leaf] = new Encoder[Leaf] {
    final def apply(ast: Leaf): Json = ast match {
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
    Decoder.forProduct3("id", "constant", "metadata")(ScalarSource.apply)
  implicit lazy val encodeConstant: Encoder[ScalarSource] =
    Encoder.forProduct4("type", "id", "constant", "metadata")(op => (op.`type`, op.id, op.constant, op.metadata))
}

