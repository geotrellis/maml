package com.azavea.maml.ast

import MamlKind._
import com.azavea.maml.ast.utility._

import java.security.InvalidParameterException


/** Operations which should only have one argument. */
trait BinaryExpression extends Expression {
  require(children.length == 2, s"Incorrect number of arguments to a binary expression. Expected 2, found ${children.length}")
  val kindDerivation: (MamlKind, MamlKind) => MamlKind
  lazy val kind = this.children.map({ _.kind }).reduce({ kindDerivation(_, _) })
}

object BinaryExpression {
  def scalarCompareDerivation(k1: MamlKind, k2: MamlKind): MamlKind = (k1, k2) match {
    case (MamlKind.Tile, MamlKind.Tile) => MamlKind.Tile
    case (MamlKind.Tile, MamlKind.Int) => MamlKind.Tile
    case (MamlKind.Tile, MamlKind.Double) => MamlKind.Tile
    case (MamlKind.Int, MamlKind.Tile) => MamlKind.Tile
    case (MamlKind.Double, MamlKind.Tile) => MamlKind.Tile
    case (MamlKind.Int, MamlKind.Int) => MamlKind.Bool
    case (MamlKind.Double, MamlKind.Double) => MamlKind.Bool
    case (x1, x2) => throw new InvalidParameterException(s"Expected tile or scalar kinds. Found $x1 and $x2")
  }
}

case class Masking(children: List[Expression]) extends Operation with BinaryExpression {
  val kindDerivation = { (k1: MamlKind, k2: MamlKind) =>
    (k1, k2) match {
      case (MamlKind.Tile, MamlKind.Geom) => MamlKind.Tile
      case (MamlKind.Geom, MamlKind.Tile) => MamlKind.Tile
      case (x1, x2) => throw new InvalidParameterException(s"Expected tile and geometry kinds. Found $x1 and $x2")
    }
  }

  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Less(children: List[Expression]) extends Operation with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class LessOrEqual(children: List[Expression]) extends Operation with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Equal(children: List[Expression]) extends Operation with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class GreaterOrEqual(children: List[Expression]) extends Operation with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Greater(children: List[Expression]) extends Operation with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Or(children: List[Expression]) extends Operation with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Xor(children: List[Expression]) extends Operation with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class And(children: List[Expression]) extends Operation with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

