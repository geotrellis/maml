package com.azavea.maml.ast

import java.security.InvalidParameterException

trait FoldableExpression { expression: Expression =>
  require(expression.children.length > 1, s"Incorrect number of arguments to a foldable expression. Expected >1, found ${expression.children.length}")
  val kindDerivation: (MamlKind, MamlKind) => MamlKind
  lazy val kind = this.children.map { _.kind }.reduce { kindDerivation(_, _) }
}

object FoldableExpression {
  def imageOrScalarDerivation(exp: FoldableExpression)(k1: MamlKind, k2: MamlKind): MamlKind = (k1, k2) match {
    case (MamlKind.Image, MamlKind.Image)   => MamlKind.Image
    case (MamlKind.Int, MamlKind.Int)       => MamlKind.Int
    case (MamlKind.Image, MamlKind.Int)     => MamlKind.Image
    case (MamlKind.Int, MamlKind.Image)     => MamlKind.Image
    case (MamlKind.Double, MamlKind.Double) => MamlKind.Double
    case (MamlKind.Image, MamlKind.Double)  => MamlKind.Image
    case (MamlKind.Double, MamlKind.Image)  => MamlKind.Image
    case (MamlKind.Double, MamlKind.Int)    => MamlKind.Double
    case (MamlKind.Int, MamlKind.Double)    => MamlKind.Double
    case (x1, x2)                           => throw new InvalidParameterException(s"Expected image, int, or double kind. Found $x1 $x2")
  }

  def imageOnly(exp: FoldableExpression)(k1: MamlKind, k2: MamlKind): MamlKind = (k1, k2) match {
    case (MamlKind.Image, MamlKind.Image) => MamlKind.Image
    case (x1, x2)                         => throw new InvalidParameterException(s"Expected image kind. Found $x1 $x2")
  }
}
