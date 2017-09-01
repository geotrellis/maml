package maml.ast

import MamlKind._
import maml.ast.utility._


/** Operations which should only have one argument. */
trait UnaryExpression[T] extends Expression[T] {
  require(children.length == 1, s"Incorrect number of arguments to a unary expression. Expected 1, found ${children.length}")
  require(kindDerivation.keys.toList.contains(children.head.kind), s"TypeError: ${this} expected one of ${kindDerivation.keys.toList}, found ${children.head.kind}")
  lazy val kind = kindDerivation(children.head.kind)
  def kindDerivation: Map[MamlKind, MamlKind]
}

case class Classification[T](children: List[Expression[T]], classMap: ClassMap, extra: T) extends Operation[T] with UnaryExpression[T] {
  val kindDerivation: Map[MamlKind, MamlKind] = Map(MamlKind.Tile -> MamlKind.Tile)
  def withChildren(newChildren: List[Expression[T]]): Expression[T] = copy(children = newChildren)
}
