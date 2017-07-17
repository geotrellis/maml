package maml.ast.codec.tree

import maml.ast._
import maml.ast.codec._

import io.circe._
import io.circe.syntax._

import java.security.InvalidParameterException


trait MamlTreeCodec
    extends MamlOperationCodecs
       with MamlSourceCodecs
       with MamlUtilityCodecs {

  /** TODO: Add codec paths besides `raster source` and `operation` when supported */
  implicit def mamlDecoder = Decoder.instance[MamlTree] { ma =>
    ma._type match {
      case Some(_) =>
        ma.as[Operation]
      case None =>
        ma.as[Source]
    }
  }

  implicit def mamlEncoder: Encoder[MamlTree] = new Encoder[MamlTree] {
    final def apply(ast: MamlTree): Json = ast match {
      case operation: Operation =>
        operation.asJson
      case src: Source =>
        src.asJson
      case _ =>
        throw new InvalidParameterException(s"Unrecognized AST: $ast")
    }
  }
}

object MamlTreeCodec extends MamlTreeCodec
