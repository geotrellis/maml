package maml.rdd.ast.codec

import maml.ast._
import maml.ast.codec._
import maml.rdd.ast._

import io.circe._
import io.circe.syntax._
import io.circe.generic.extras.semiauto._
import io.circe.generic.extras.Configuration

import java.security.InvalidParameterException


trait RDDMamlSourceCodecs extends RDDMamlUtilityCodecs {
  implicit def conf: Configuration
  implicit def rddMamlDecoder: Decoder[Expression]
  implicit def rddMamlEncoder: Encoder[Expression]

  implicit lazy val decodeSpatialRDDSource: Decoder[SpatialRDDSource] = deriveDecoder
  implicit lazy val encodeSpatialRDDSource: Encoder[SpatialRDDSource] = deriveEncoder
}
