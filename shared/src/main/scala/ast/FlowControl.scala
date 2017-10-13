package com.azavea.maml.ast

import io.circe.generic.JsonCodec

case class Branch(children: List[Expression]) extends Operation {
  require(children.length == 3, s"Incorrect number of arguments to a branching/if-else expression. Expected 3, found ${children.length}")
  lazy val kind = children(1).kind
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}
