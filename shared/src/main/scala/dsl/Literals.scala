package com.azavea.maml.dsl

import com.azavea.maml.ast._

trait Literals {
  implicit def intIsIntLiteral(int: Int): IntLit = IntLit(int)
  implicit def dblIsDoubleLiteral(dbl: Double): DblLit = DblLit(dbl)
  implicit def boolIsBoolLiteral(bool: Boolean): BoolLit = BoolLit(bool)
}
