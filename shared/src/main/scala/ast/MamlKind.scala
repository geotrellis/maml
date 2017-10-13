package com.azavea.maml.ast

sealed trait MamlKind
object MamlKind {
  case object Bool extends MamlKind
  case object Scalar extends MamlKind
  case object Tile extends MamlKind
  case object Geom extends MamlKind
}
