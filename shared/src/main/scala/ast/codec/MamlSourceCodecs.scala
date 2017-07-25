package maml.ast.codec

import maml.ast._

import io.circe._
import io.circe.syntax._

import java.security.InvalidParameterException


trait MamlSourceCodecs extends MamlUtilityCodecs {
  implicit def mamlDecoder: Decoder[Expression]
  implicit def mamlEncoder: Encoder[Expression]

  /** TODO: Add codec paths besides `raster source` and `operation` when supported */
  implicit val mamlSourceDecoder = Decoder.instance[Source] { ma =>
    ma.kind match {
      case Some("tile") =>
        Right(TileSource)
      case Some("scalar") =>
        ma.as[ScalarSource]
      case Some(unrecognized) =>
        Left(DecodingFailure(s"Unrecognized Source node with kind: $ma", ma.history))
      case None =>
        Left(DecodingFailure(s"Unrecognized Source node: $ma", ma.history))
    }
  }

  implicit val mamlSourceEncoder: Encoder[Source] = new Encoder[Source] {
    final def apply(ast: Source): Json = ast match {
      case t@TileSource =>
        t.asJson
      case scalar: ScalarSource =>
        scalar.asJson
      case _ =>
        throw new InvalidParameterException(s"Unrecognized AST: $ast")
    }
  }

  implicit lazy val encodeSource: Encoder[TileSource.type] =
    Encoder.forProduct1("kind")(op => (op.kind))

  implicit lazy val decodeScalar: Decoder[ScalarSource] =
    Decoder.forProduct1("scalar")(ScalarSource.apply)
  implicit lazy val encodeScalar: Encoder[ScalarSource] =
    Encoder.forProduct2("kind", "scalar")(op => (op.kind, op.scalar))
}

