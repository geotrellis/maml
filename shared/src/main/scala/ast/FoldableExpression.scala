package maml.ast

import maml.ast.utility._
import MamlKind._


trait FoldableExpression extends Expression with Serializable {
  val symbol: String
  val children: List[Expression]
  def withChildren(newChildren: List[Expression]): Expression

  def kind: MamlKind = {
    val kinds = children.map(_.kind)
    if (kinds.contains(MamlTile)) MamlTile
    else MamlScalar
  }
}


case class Addition(children: List[Expression]) extends FoldableExpression {
  val symbol: String = "+"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Subtraction(children: List[Expression]) extends FoldableExpression {
  val symbol: String = "-"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Multiplication(children: List[Expression]) extends FoldableExpression {
  val symbol = "*"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Division(children: List[Expression]) extends FoldableExpression {
  val symbol: String = "/"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Max(children: List[Expression]) extends FoldableExpression {
  val symbol: String = "max"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Min(children: List[Expression]) extends FoldableExpression {
  val symbol: String = "min"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}


