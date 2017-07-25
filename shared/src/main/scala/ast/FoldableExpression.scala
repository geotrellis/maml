package maml.ast

import maml.ast.utility._
import maml.error._

import cats.data._
import Validated._


trait FoldableExpression extends Operation {
  val symbol: String
  val children: List[Expression]
  def withChildren(newChildren: List[Expression]): Expression

  val kindDerivation: (MamlKind, MamlKind) => Interpreted[MamlKind]

}

object FoldableExpression {
  def tileOrScalarDerivation(exp: FoldableExpression)(k1: MamlKind, k2: MamlKind): Interpreted[MamlKind] = (k1, k2) match {
    case (MamlKind.Tile, MamlKind.Tile) => Valid(MamlKind.Tile)
    case (MamlKind.Scalar, MamlKind.Scalar) => Valid(MamlKind.Scalar)
    case (MamlKind.Tile, MamlKind.Scalar) => Valid(MamlKind.Tile)
    case (MamlKind.Scalar, MamlKind.Tile) => Valid(MamlKind.Tile)
    case (k3, k4) => Invalid(NonEmptyList.of(FoldableTypeError(exp, (k1, k2))))
  }
}



case class Addition(children: List[Expression]) extends FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  val symbol: String = "+"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Subtraction(children: List[Expression]) extends FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  val symbol: String = "-"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Multiplication(children: List[Expression]) extends FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  val symbol = "*"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Division(children: List[Expression]) extends FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  val symbol: String = "/"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Max(children: List[Expression]) extends FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  val symbol: String = "max"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Min(children: List[Expression]) extends FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  val symbol: String = "min"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}


