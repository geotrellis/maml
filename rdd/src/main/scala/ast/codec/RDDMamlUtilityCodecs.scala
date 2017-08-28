package maml.rdd.ast.codec

import maml.rdd.eval._
import maml.rdd.ast._
import maml.ast._
import maml.ast.utility._

import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.optics.JsonPath._
import cats.syntax.functor._

import geotrellis.spark._


trait RDDMamlUtilityCodecs {
  implicit val rddMamlKindDecoder: Decoder[MamlKind] = Decoder[String].emap({
    case "spatialRDD" => Right(MamlKind.Tile)
  })

  implicit val rddMamlKindEncoder: Encoder[MamlKind] =
    Encoder.encodeString.contramap[MamlKind]({ mk =>
      mk match {
        case MamlKind.Tile => "SpatialRDD"
      }
    })
}

object RDDMamlUtilityCodecs extends RDDMamlUtilityCodecs
