package com.azavea.maml.ast

/** Operations which should only have one argument. */
trait UnaryExpression { expression: Expression =>
  require(expression.children.length == 1, s"Incorrect number of arguments to a unary expression. Expected 1, found ${expression.children.length}")
  lazy val kind = kindDerivation(expression.children.head.kind)
  def kindDerivation: Map[MamlKind, MamlKind]
}

object UnaryExpression {
  val imageOnly: Map[MamlKind, MamlKind] = Map(MamlKind.Image -> MamlKind.Image)
  val intOnly: Map[MamlKind, MamlKind] = Map(MamlKind.Int -> MamlKind.Int)
  val dblOnly: Map[MamlKind, MamlKind] = Map(MamlKind.Double -> MamlKind.Double)
  val boolOnly: Map[MamlKind, MamlKind] = Map(MamlKind.Bool -> MamlKind.Bool)
  val scalar = intOnly ++ dblOnly
  val imageOrScalar = imageOnly ++ intOnly ++ dblOnly
}

