package maml.ast

import maml.ast.utility._
import maml.error._

import cats.data._
import Validated._

import java.security.InvalidParameterException

trait FoldableExpression[T] extends Expression[T] {
  require(children.length > 1, s"Incorrect number of arguments to a foldable expression. Expected >1, found ${children.length}")
  val kindDerivation: (MamlKind, MamlKind) => MamlKind
  lazy val kind = this.children.map({ _.kind }).reduce({ kindDerivation(_, _) })
}

object FoldableExpression {
  def tileOrScalarDerivation[T](exp: FoldableExpression[T])(k1: MamlKind, k2: MamlKind): MamlKind = (k1, k2) match {
    case (MamlKind.Tile, MamlKind.Tile) => MamlKind.Tile
    case (MamlKind.Int, MamlKind.Int) => MamlKind.Int
    case (MamlKind.Tile, MamlKind.Int) => MamlKind.Tile
    case (MamlKind.Int, MamlKind.Tile) => MamlKind.Tile
    case (MamlKind.Double, MamlKind.Double) => MamlKind.Double
    case (MamlKind.Tile, MamlKind.Double) => MamlKind.Tile
    case (MamlKind.Double, MamlKind.Tile) => MamlKind.Tile
    case (MamlKind.Double, MamlKind.Int) => MamlKind.Double
    case (MamlKind.Int, MamlKind.Double) => MamlKind.Double
    case (x1, x2) => throw new InvalidParameterException(s"Expected tile, int, or double kind. Found $x1 $x2")
  }
}

case class Addition[T](children: List[Expression[T]], extra: T) extends Operation[T] with FoldableExpression[T] {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression[T]]): Expression[T] = copy(children = newChildren)
}

case class Subtraction[T](children: List[Expression[T]], extra: T) extends Operation[T] with FoldableExpression[T] {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression[T]]): Expression[T] = copy(children = newChildren)
}

case class Multiplication[T](children: List[Expression[T]], extra: T) extends Operation[T] with FoldableExpression[T] {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression[T]]): Expression[T] = copy(children = newChildren)
}

case class Division[T](children: List[Expression[T]], extra: T) extends Operation[T] with FoldableExpression[T] {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression[T]]): Expression[T] = copy(children = newChildren)
}

case class Max[T](children: List[Expression[T]], extra: T) extends Operation[T] with FoldableExpression[T] {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression[T]]): Expression[T] = copy(children = newChildren)
}

case class Min[T](children: List[Expression[T]], extra: T) extends Operation[T] with FoldableExpression[T] {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression[T]]): Expression[T] = copy(children = newChildren)
}
