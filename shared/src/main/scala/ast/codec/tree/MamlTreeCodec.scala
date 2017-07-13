package maml.ast.codec.tree

import maml.ast._
import maml.ast.codec._
import maml.ast.leaf.Leaf
import maml.ast.operation.Operation

import io.circe._
import io.circe.syntax._

import java.security.InvalidParameterException


trait MamlTreeCodec
    extends MamlOperationCodecs
       with MamlLeafCodecs
       with MamlUtilityCodecs {

  /** TODO: Add codec paths besides `raster source` and `operation` when supported */
  implicit def mamlDecoder = Decoder.instance[MamlTree] { ma =>
    ma._symbol match {
      case Some(_) =>
        ma.as[Operation]
      case None =>
        ma.as[Leaf]
    }
  }

  implicit def mamlEncoder: Encoder[MamlTree] = new Encoder[MamlTree] {
    final def apply(ast: MamlTree): Json = ast match {
      case operation: Operation =>
        operation.asJson
      case leaf: Leaf =>
        leaf.asJson
      case _ =>
        throw new InvalidParameterException(s"Unrecognized AST: $ast")
    }
  }
}

object MamlTreeCodec extends MamlTreeCodec
