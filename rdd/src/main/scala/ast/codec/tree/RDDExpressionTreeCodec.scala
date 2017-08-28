package maml.rdd.ast.codec.tree

import maml.ast._
import maml.ast.codec._
import maml.rdd.ast._
import maml.rdd.ast.codec._
import maml.rdd.ast.codec.tree._

import io.circe._
import io.circe.syntax._
import io.circe.generic.extras.Configuration

import java.security.InvalidParameterException


trait RDDExpressionTreeCodec
  extends RDDMamlSourceCodecs
  with RDDMamlUtilityCodecs {

  implicit def conf: Configuration =
    Configuration.default.withDefaults.withDiscriminator("type")

  implicit def rddMamlDecoder = Decoder.instance[Expression] { ma =>
    ma._type match {
      case Some("SpatialRDDSource") => ma.as[SpatialRDDSource]
      case _ => Left(DecodingFailure(s"Unrecognized node: $ma", ma.history))
    }
  }

  implicit def rddMamlEncoder: Encoder[Expression] = new Encoder[Expression] {
    final def apply(ast: Expression): Json =
      ast match {
        case node: SpatialRDDSource => node.asJson
        case _ => throw new InvalidParameterException(s"Unrecognized AST: $ast")
      }
  }
}

object RDDExpressionTreeCodec extends RDDExpressionTreeCodec with RDDMamlSourceCodecs
