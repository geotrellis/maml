package maml.ast

import MamlKind._
import maml.ast.utility._


/** Operations which should only have one argument. */
trait UnaryExpression extends Expression with Product with Serializable {
  require(children.length == 0, "too many children for unary expression")
  val symbol: String
}

case class Classification(children: List[Expression], classMap: ClassMap) extends UnaryExpression {
  require(children.head.kind == MamlTile, s"TypeError: found type ${children.head.kind}, expected type MamlTile")

  val symbol: String = "classify"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
  val kind: MamlKind = children.head.kind
}

case class Masking(children: List[Expression], mask: MultiPolygon) extends UnaryExpression {
  require(children.head.kind == MamlTile, s"TypeError: found type ${children.head.kind}, expected type MamlTile")

  val symbol: String = "mask"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
  val kind: MamlKind = children.head.kind
}
