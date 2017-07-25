package maml.ast

import MamlKind._
import maml.ast.utility._


/** Operations which should only have one argument. */
trait UnaryExpression extends Operation {
  require(children.length == 1, "too many children for unary expression")
  val expectedKind: Map[MamlKind, MamlKind]
  val symbol: String
}

case class Classification(children: List[Expression], classMap: ClassMap) extends UnaryExpression {
  val expectedKind: Map[MamlKind, MamlKind] = Map(MamlKind.Tile -> MamlKind.Tile)
  val symbol: String = "classify"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Masking(children: List[Expression], mask: MultiPolygon) extends UnaryExpression {
  val expectedKind: Map[MamlKind, MamlKind] = Map(MamlKind.Tile -> MamlKind.Tile)
  val symbol: String = "mask"
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}
