package maml.ast

import MamlKind._
import maml.ast._
import maml.ast.utility._


// TODO maybe don't use lists for these unary things
trait FocalExpression[T] extends UnaryExpression[T] {
  def neighborhood: Neighborhood
  def kindDerivation: Map[MamlKind, MamlKind] = Map(MamlKind.Tile -> MamlKind.Tile)
}

case class FocalMax[T](children: List[Expression[T]], neighborhood: Neighborhood, extra: T) extends Operation[T] with FocalExpression[T] {
  def withChildren(newChildren: List[Expression[T]]): Expression[T] = copy(children = newChildren)
}

case class FocalMin[T](children: List[Expression[T]], neighborhood: Neighborhood, extra: T) extends Operation[T] with FocalExpression[T] {
  def withChildren(newChildren: List[Expression[T]]): Expression[T] = copy(children = newChildren)
}

case class FocalMean[T](children: List[Expression[T]], neighborhood: Neighborhood, extra: T) extends Operation[T] with FocalExpression[T] {
  def withChildren(newChildren: List[Expression[T]]): Expression[T] = copy(children = newChildren)
}

case class FocalMedian[T](children: List[Expression[T]], neighborhood: Neighborhood, extra: T) extends Operation[T] with FocalExpression[T] {
  def withChildren(newChildren: List[Expression[T]]): Expression[T] = copy(children = newChildren)
}

case class FocalMode[T](children: List[Expression[T]], neighborhood: Neighborhood, extra: T) extends Operation[T] with FocalExpression[T] {
  def withChildren(newChildren: List[Expression[T]]): Expression[T] = copy(children = newChildren)
}

case class FocalSum[T](children: List[Expression[T]], neighborhood: Neighborhood, extra: T) extends Operation[T] with FocalExpression[T] {
  def withChildren(newChildren: List[Expression[T]]): Expression[T] = copy(children = newChildren)
}

case class FocalStdDev[T](children: List[Expression[T]], neighborhood: Neighborhood, extra: T) extends Operation[T] with FocalExpression[T] {
  def withChildren(newChildren: List[Expression[T]]): Expression[T] = copy(children = newChildren)
}
