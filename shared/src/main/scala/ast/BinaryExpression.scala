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
    case (MamlKind.Image, MamlKind.Image) => MamlKind.Image
    case (MamlKind.Image, MamlKind.Int) => MamlKind.Image
    case (MamlKind.Image, MamlKind.Double) => MamlKind.Image
    case (MamlKind.Int, MamlKind.Image) => MamlKind.Image
    case (MamlKind.Double, MamlKind.Image) => MamlKind.Image
    case (MamlKind.Int, MamlKind.Int) => MamlKind.Bool
    case (MamlKind.Double, MamlKind.Double) => MamlKind.Bool
    case (x1, x2) => throw new InvalidParameterException(s"Expected image or scalar kinds. Found $x1 and $x2")
  }
}

