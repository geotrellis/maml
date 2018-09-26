package com.azavea.maml.ast

import java.security.InvalidParameterException


/** Operations which should only have two arguments. */
trait BinaryExpression { expression: Expression =>
  require(children.length == 2, s"Incorrect number of arguments to a binary expression. Expected 2, found ${children.length}")
  val kindDerivation: (MamlKind, MamlKind) => MamlKind
  lazy val kind = expression.children.map({ _.kind }).reduce({ kindDerivation(_, _) })
}

object BinaryExpression {
  def scalarCompareDerivation(k1: MamlKind, k2: MamlKind): MamlKind = (k1, k2) match {
    case (MamlKind.Tile, MamlKind.Tile) => MamlKind.Tile
    case (MamlKind.Tile, MamlKind.Int) => MamlKind.Tile
    case (MamlKind.Tile, MamlKind.Double) => MamlKind.Tile
    case (MamlKind.Int, MamlKind.Tile) => MamlKind.Tile
    case (MamlKind.Double, MamlKind.Tile) => MamlKind.Tile
    case (MamlKind.Int, MamlKind.Int) => MamlKind.Bool
    case (MamlKind.Double, MamlKind.Double) => MamlKind.Bool
    case (x1, x2) => throw new InvalidParameterException(s"Expected tile or scalar kinds. Found $x1 and $x2")
  }
}

