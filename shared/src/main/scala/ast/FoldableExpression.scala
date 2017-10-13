package com.azavea.maml.ast

import com.azavea.maml.util._

import cats.data._
import Validated._

import java.security.InvalidParameterException

trait FoldableExpression extends Expression {
  require(children.length > 1, s"Incorrect number of arguments to a foldable expression. Expected >1, found ${children.length}")
  val kindDerivation: (MamlKind, MamlKind) => MamlKind
  lazy val kind = this.children.map({ _.kind }).reduce({ kindDerivation(_, _) })
}

object FoldableExpression {
  def tileOrScalarDerivation(exp: FoldableExpression)(k1: MamlKind, k2: MamlKind): MamlKind = (k1, k2) match {
    case (MamlKind.Tile, MamlKind.Tile) => MamlKind.Tile
    case (MamlKind.Scalar, MamlKind.Scalar) => MamlKind.Scalar
    case (MamlKind.Tile, MamlKind.Scalar) => MamlKind.Tile
    case (MamlKind.Scalar, MamlKind.Tile) => MamlKind.Tile
    case (MamlKind.Scalar, MamlKind.Scalar) => MamlKind.Scalar
    case (MamlKind.Tile, MamlKind.Scalar) => MamlKind.Tile
    case (MamlKind.Scalar, MamlKind.Tile) => MamlKind.Tile
    case (MamlKind.Scalar, MamlKind.Scalar) => MamlKind.Scalar
    case (MamlKind.Scalar, MamlKind.Scalar) => MamlKind.Scalar
    case (x1, x2) => throw new InvalidParameterException(s"Expected tile, int, or double kind. Found $x1 $x2")
  }
}

case class Addition(children: List[Expression]) extends Operation with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Subtraction(children: List[Expression]) extends Operation with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Multiplication(children: List[Expression]) extends Operation with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Division(children: List[Expression]) extends Operation with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Max(children: List[Expression]) extends Operation with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Min(children: List[Expression]) extends Operation with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

