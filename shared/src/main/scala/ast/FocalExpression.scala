package com.azavea.maml.ast

import MamlKind._
import com.azavea.maml.ast._
import com.azavea.maml.ast.utility._


// TODO maybe don't use lists for these unary things
trait FocalExpression extends UnaryExpression {
  def neighborhood: Neighborhood
  def kindDerivation: Map[MamlKind, MamlKind] = Map(MamlKind.Tile -> MamlKind.Tile)
}

case class FocalMax(children: List[Expression], neighborhood: Neighborhood) extends Operation with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalMin(children: List[Expression], neighborhood: Neighborhood) extends Operation with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalMean(children: List[Expression], neighborhood: Neighborhood) extends Operation with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalMedian(children: List[Expression], neighborhood: Neighborhood) extends Operation with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalMode(children: List[Expression], neighborhood: Neighborhood) extends Operation with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalSum(children: List[Expression], neighborhood: Neighborhood) extends Operation with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalStdDev(children: List[Expression], neighborhood: Neighborhood) extends Operation with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

