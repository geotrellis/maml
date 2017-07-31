package maml.ast

import maml.ast.utility._
import maml.error._

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
    case (x1, x2) => throw new InvalidParameterException(s"Expected tile or scalar kind. Found $x1 $x2")
  }

  def tileOrBoolDerivation(exp: FoldableExpression)(k1: MamlKind, k2: MamlKind): MamlKind = (k1, k2) match {
    case (MamlKind.Tile, MamlKind.Tile) => MamlKind.Tile
    case (MamlKind.Bool, MamlKind.Bool) => MamlKind.Bool
    case (MamlKind.Tile, MamlKind.Bool) => MamlKind.Tile
    case (MamlKind.Bool, MamlKind.Tile) => MamlKind.Tile
    case (x1, x2) => throw new InvalidParameterException(s"Expected tile or bool kind. Found $x1 $x2")
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

case class Less(children: List[Expression]) extends Operation with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrBoolDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class LessOrEqual(children: List[Expression]) extends Operation with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrBoolDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Equal(children: List[Expression]) extends Operation with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrBoolDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class GreaterOrEqual(children: List[Expression]) extends Operation with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrBoolDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Greater(children: List[Expression]) extends Operation with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrBoolDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Or(children: List[Expression]) extends Operation with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrBoolDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Xor(children: List[Expression]) extends Operation with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrBoolDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class And(children: List[Expression]) extends Operation with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrBoolDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

