package com.azavea.maml.ast

import io.circe.generic.JsonCodec

case class Branch(children: List[Expression]) extends Expression {
  require(children.length == 3, s"Incorrect number of arguments to a branching/if-else expression. Expected 3, found ${children.length}")
  require(children(0).kind == MamlKind.Bool, s"The first argument to branching/if-else must have Kind Bool. Found ${children(0).kind}")
  require(children(1).kind == children(2).kind, s"Unable to determine branching/if-else kind. If and Else body must be of the same kind. If-body: ${children(1).kind}. Else-body: ${children(2).kind}")
  lazy val kind = children(1).kind
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}
