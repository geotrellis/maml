package com.azavea.maml.ast

import io.circe._
import cats._
import cats.implicits._

import scala.util.Try

sealed trait MamlKind { def repr: String }
object MamlKind {
  case object Bool extends MamlKind { def repr: String = "bool" }
  case object Int extends MamlKind { def repr: String = "int" }
  case object Double extends MamlKind { def repr: String = "double" }
  case object Image extends MamlKind { def repr: String = "img" }
  case object Geom extends MamlKind { def repr: String = "geom" }
  // The bottom type [useful for writing inductive proofs/handling certain classes of failure]
  case object Nothing extends MamlKind { def repr: String = "nothing" }

  def fromString(str: String): Option[MamlKind] = Try {
    str match {
      case "bool"    => MamlKind.Bool
      case "int"     => MamlKind.Int
      case "double"  => MamlKind.Double
      case "img"     => MamlKind.Image
      case "geom"    => MamlKind.Geom
      case "nothing" => MamlKind.Nothing
    }
  }.toOption

  implicit lazy val decodeMamlKind: Decoder[MamlKind] = Decoder.decodeString.emap { str =>
    Either.catchNonFatal(fromString(str).get).leftMap(t => "MamlKind")
  }

  implicit lazy val encodeMamlKind: Encoder[MamlKind] =
    Encoder.encodeString.contramap[MamlKind](_.repr)
}
