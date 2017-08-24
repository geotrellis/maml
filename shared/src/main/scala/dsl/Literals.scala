package maml.dsl

import maml.ast._


trait Literals {
  implicit def intIsIntLiteral(int: Int): IntLiteral = IntLiteral(int)
  implicit def dblIsDoubleLiteral(dbl: Double): DoubleLiteral = DoubleLiteral(dbl)
  implicit def boolIsBoolLiteral(bool: Boolean): BoolLiteral = BoolLiteral(bool)
}
