package maml.ast

import io.circe.generic.JsonCodec


/** The ur-type for a recursive representation of MapAlgebra operations */
trait Operation extends Expression {
  def children: Seq[Expression]

  def sources: Seq[Source] = children.flatMap(_.sources).distinct
}

