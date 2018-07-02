package com.azavea.maml.ast

import io.circe.generic.JsonCodec


case class ExpressionPath(indices: Seq[Int])

/** The ur-type for a recursive representation of MapAlgebra operations */
trait Expression extends Product with Serializable {
  def children: List[Expression]
  def sources: Set[Source]
  def unboundSources: Set[UnboundSource]
  def boundSources: Set[BoundSource]
  def kind: MamlKind
  def withChildren(newChildren: List[Expression]): Expression
}

