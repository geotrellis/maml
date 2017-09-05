package maml.ast

import io.circe._
import io.circe.generic.extras.semiauto._


case class ExpressionPath(indices: Seq[Int])

/** The ur-type for a recursive representation of MapAlgebra operations */
trait Expression[T] extends Product with Serializable {
  def children: List[Expression[T]]
  def sources: List[Source[T]]
  def kind: MamlKind
  def withChildren(newChildren: List[Expression[T]]): Expression[T]

  /** Extra information that can be stuffed into any node of an Expression tree. */
  val extra: T
}

object Expression {
  // implicit def expDecoder[T: Decoder]: Decoder[Expression[T]] = deriveDecoder
  // implicit def expEncoder[T: Encoder]: Encoder[Expression[T]] = deriveEncoder
}
