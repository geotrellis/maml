package maml.ast

import MamlKind._
import maml.ast.utility._


/** Operations which should only have one argument. */
trait UnaryExpression extends Expression {
  require(children.length == 1, s"Incorrect number of arguments to a unary expression. Expected 1, found ${children.length}")
  require(kindDerivation.keys.toList.contains(children.head.kind), s"TypeError: ${this} expected one of ${kindDerivation.keys.toList}, found ${children.head.kind}")
  lazy val kind = kindDerivation(children.head.kind)

  def kindDerivation: Map[MamlKind, MamlKind]
}

case class Classification(children: List[Expression], classMap: ClassMap) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = Map(MamlKind.Tile -> MamlKind.Tile)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Masking(children: List[Expression], mask: MultiPolygon) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = Map(MamlKind.Tile -> MamlKind.Tile)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}
