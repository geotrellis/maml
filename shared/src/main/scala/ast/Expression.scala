package com.azavea.maml.ast

import com.azavea.maml.util._
import com.azavea.maml.error._
import com.azavea.maml.eval.Interpreted
import com.azavea.maml.util.NeighborhoodConversion

import cats._
import cats.data.{NonEmptyList => NEL, _}; import Validated._
import cats.implicits._

import java.security.InvalidParameterException


object Expression {
  def vars(expr: Expression): Map[String, MamlKind] =
    varsWithBuffer(expr).map { case (name, (kind, _)) => name -> kind }

  def varsWithBuffer(expr: Expression): Map[String, (MamlKind, Int)] = {
    def eval(subExp: Expression, buffer: Int): List[(String, MamlKind, Int)] = subExp match {
      case v: Variable =>
        List((v.name, v.kind, buffer))
      case s: Source =>
        List()
      case f: Expression with FocalExpression =>
        subExp.children
          .flatMap(eval(_, buffer + NeighborhoodConversion(f.neighborhood).extent))
      case _ =>
        subExp.children
          .flatMap(eval(_, buffer))
    }
    // max by the buffer to ensure that we have enough data for all operations
    eval(expr, 0)
      .groupBy(_._1)
      .mapValues({ values => values.maxBy(_._3) })
      .map({ case (name, (_, kind, buffer)) => name -> (kind, buffer) })
      .toMap
  }

  def bindParams(expr: Expression, params: Map[String, Literal]): Interpreted[Expression] = {
    def eval(subExpr: Expression): Interpreted[Expression] = subExpr match {
      case v: Variable =>
        params.get(v.name) match {
          case Some(literal) if literal.asInstanceOf[Expression].kind == subExpr.kind => Valid(literal.asInstanceOf[Expression])
          case Some(literal) => Invalid(NEL.of(DivergingTypes(literal.asInstanceOf[Expression].kind.toString, List(subExpr.kind.toString))))
          case None => Invalid(NEL.of(NoVariableBinding(v, params)))
        }
      case _ =>
        subExpr.children.map(eval(_)).sequence.map(subExpr.withChildren)
    }
    eval(expr)
  }


}

/** The ur-type for a recursive representation of MapAlgebra operations */
sealed trait Expression extends Product with Serializable {
  def children: List[Expression]
  def kind: MamlKind
  def withChildren(newChildren: List[Expression]): Expression

  def bind(args: Map[String, Literal]): Interpreted[Expression] =
    children.map(_.bind(args)).sequence.map(this.withChildren)
}

case class Addition(children: List[Expression]) extends Expression with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Subtraction(children: List[Expression]) extends Expression with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Multiplication(children: List[Expression]) extends Expression with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Division(children: List[Expression]) extends Expression with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Max(children: List[Expression]) extends Expression with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Min(children: List[Expression]) extends Expression with FoldableExpression {
  val kindDerivation = FoldableExpression.tileOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}


case class Masking(children: List[Expression]) extends Expression with BinaryExpression {
  val kindDerivation = { (k1: MamlKind, k2: MamlKind) =>
    (k1, k2) match {
      case (MamlKind.Tile, MamlKind.Geom) => MamlKind.Tile
      case (MamlKind.Geom, MamlKind.Tile) => MamlKind.Tile
      case (x1, x2) => throw new InvalidParameterException(s"Expected tile and geometry kinds. Found $x1 and $x2")
    }
  }

  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Pow(children: List[Expression]) extends Expression with BinaryExpression {
  val kindDerivation = { (k1: MamlKind, k2: MamlKind) =>
    (k1, k2) match {
      case (MamlKind.Tile, MamlKind.Tile) => MamlKind.Tile
      case (MamlKind.Tile, MamlKind.Int) => MamlKind.Tile
      case (MamlKind.Int, MamlKind.Tile) => MamlKind.Tile
      case (MamlKind.Tile, MamlKind.Double) => MamlKind.Tile
      case (MamlKind.Double, MamlKind.Tile) => MamlKind.Tile
      case (MamlKind.Double, MamlKind.Int) => MamlKind.Double
      case (MamlKind.Int, MamlKind.Double) => MamlKind.Double
      case (MamlKind.Int, MamlKind.Int) => MamlKind.Double
      case (MamlKind.Double, MamlKind.Double) => MamlKind.Double
      case (x1, x2) => throw new InvalidParameterException(s"Expected tile and scalar kinds. Found $x1 and $x2")
    }
  }
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Less(children: List[Expression]) extends Expression with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class LessOrEqual(children: List[Expression]) extends Expression with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Unequal(children: List[Expression]) extends Expression with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Equal(children: List[Expression]) extends Expression with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class GreaterOrEqual(children: List[Expression]) extends Expression with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Greater(children: List[Expression]) extends Expression with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Or(children: List[Expression]) extends Expression with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Xor(children: List[Expression]) extends Expression with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class And(children: List[Expression]) extends Expression with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Atan2(children: List[Expression]) extends Expression with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

/** FLOW CONTROL */
case class Branch(children: List[Expression]) extends Expression {
  require(children.length == 3, s"Incorrect number of arguments to a branching/if-else expression. Expected 3, found ${children.length}")
  require(children(0).kind == MamlKind.Bool, s"The first argument to branching/if-else must have Kind Bool. Found ${children(0).kind}")
  require(children(1).kind == children(2).kind, s"Unable to determine branching/if-else kind. If and Else body must be of the same kind. If-body: ${children(1).kind}. Else-body: ${children(2).kind}")
  lazy val kind = children(1).kind
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

/** Operations which should only have one argument. */
case class Classification(children: List[Expression], classMap: ClassMap) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Sin(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Cos(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Tan(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Sinh(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Cosh(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Tanh(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Asin(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Acos(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Atan(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Round(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Floor(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Ceil(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

/** Natural Log */
case class LogE(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Log10(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class SquareRoot(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Abs(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Defined(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOnly
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Undefined(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOnly
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class NumericNegation(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class LogicalNegation(children: List[Expression]) extends Expression with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.tileOnly ++ UnaryExpression.boolOnly
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalMax(children: List[Expression], neighborhood: Neighborhood) extends Expression with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalMin(children: List[Expression], neighborhood: Neighborhood) extends Expression with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalMean(children: List[Expression], neighborhood: Neighborhood) extends Expression with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalMedian(children: List[Expression], neighborhood: Neighborhood) extends Expression with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalMode(children: List[Expression], neighborhood: Neighborhood) extends Expression with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalSum(children: List[Expression], neighborhood: Neighborhood) extends Expression with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalStdDev(children: List[Expression], neighborhood: Neighborhood) extends Expression with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class IntLit(value: Int) extends Expression with Literal {
  val kind = MamlKind.Int
}

case class IntVar(name: String) extends Expression with Variable {
  val kind = MamlKind.Int
}

case class DblLit(value: Double) extends Expression with Literal {
  val kind = MamlKind.Double
}

case class DblVar(name: String) extends Expression with Variable {
  val kind = MamlKind.Double
}

case class BoolLit(value: Boolean) extends Expression with Literal {
  val kind = MamlKind.Bool
}

case class BoolVar(name: String) extends Expression with Variable {
  val kind = MamlKind.Bool
}

case class GeomLit(geojson: String) extends Expression with Literal {
  val kind = MamlKind.Geom
}

case class GeomVar(name: String) extends Expression with Variable {
  val kind = MamlKind.Geom
}

case class RasterLit[A](raster: A) extends Expression with Literal {
  val kind = MamlKind.Tile
}

case class RasterVar(name: String) extends Expression with Variable {
  val kind = MamlKind.Tile
}

