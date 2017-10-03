package com.azavea.maml.ast

import MamlKind._
import com.azavea.maml.util._


/** Operations which should only have one argument. */
trait UnaryExpression extends Expression {
  require(children.length == 1, s"Incorrect number of arguments to a unary expression. Expected 1, found ${children.length}")
  require(kindDerivation.keys.toList.contains(children.head.kind), s"TypeError: ${this} expected one of ${kindDerivation.keys.toList}, found ${children.head.kind}")
  lazy val kind = kindDerivation(children.head.kind)
  def kindDerivation: Map[MamlKind, MamlKind]
}

object UnaryExpression {
  val tileOnly: Map[MamlKind, MamlKind] = Map(MamlKind.Tile -> MamlKind.Tile)
  val intOnly: Map[MamlKind, MamlKind] = Map(MamlKind.Int -> MamlKind.Int)
  val dblOnly: Map[MamlKind, MamlKind] = Map(MamlKind.Double -> MamlKind.Double)
  val boolOnly: Map[MamlKind, MamlKind] = Map(MamlKind.Bool -> MamlKind.Bool)
  val scalar = intOnly ++ dblOnly
  val tileOrScalar = tileOnly ++ intOnly ++ dblOnly
}

case class Classification(children: List[Expression], classMap: ClassMap) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Sin(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Cos(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Tan(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Sinh(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Cosh(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Tanh(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Asin(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Acos(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Atan(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Round(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Floor(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Ceil(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

/** Natural Log */
case class LogE(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Log10(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class SquareRoot(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Abs(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Defined(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOnly
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Undefined(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOnly
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class NumericNegation(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class LogicalNegation(children: List[Expression]) extends Operation with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOnly ++ UnaryExpression.boolOnly
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

