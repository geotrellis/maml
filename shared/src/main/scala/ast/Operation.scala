package maml.ast

import io.circe.generic.JsonCodec


/** The ur-type for a recursive representation of MapAlgebra operations */
trait Operation extends Expression {
  val symbol: String

  lazy val sources: List[Source] = children.flatMap(_.sources).distinct
}

