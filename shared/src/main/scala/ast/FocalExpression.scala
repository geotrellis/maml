package com.azavea.maml.ast

import MamlKind._
import com.azavea.maml.ast._
import com.azavea.maml.util._


// TODO maybe don't use lists for these unary things
trait FocalExpression extends UnaryExpression { expression: Expression =>
  def neighborhood: Neighborhood
  def kindDerivation: Map[MamlKind, MamlKind] = Map(MamlKind.Image -> MamlKind.Image)
}

