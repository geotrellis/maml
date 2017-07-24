package maml.ast

import MamlKind._
import maml.ast._
import maml.ast.utility._


trait FocalExpression extends UnaryExpression {
  require(children.head.kind == MamlTile, s"TypeError: found type ${children.head.kind}, expected type MamlTile")
  val kind = MamlTile
}

case class FocalMax(children: List[Expression], neighborhood: Neighborhood) extends FocalExpression {
  val symbol = "focalMax"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalMean(children: List[Expression], neighborhood: Neighborhood) extends FocalExpression {
  val symbol = "focalMean"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalMedian(children: List[Expression], neighborhood: Neighborhood) extends FocalExpression {
  val symbol = "focalMedian"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalMin(children: List[Expression], neighborhood: Neighborhood) extends FocalExpression {
  val symbol = "focalMin"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalMode(children: List[Expression], neighborhood: Neighborhood) extends FocalExpression {
  val symbol = "focalMode"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalSum(children: List[Expression], neighborhood: Neighborhood) extends FocalExpression {
  val symbol = "focalSum"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalStdDev(children: List[Expression], neighborhood: Neighborhood) extends FocalExpression {
  val symbol = "focalStdDev"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

