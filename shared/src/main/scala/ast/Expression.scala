package com.azavea.maml.ast

import com.azavea.maml.util._
import com.azavea.maml.error._
import geotrellis.raster.{CellType, TargetCell}
import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import cats.implicits._

import java.security.InvalidParameterException

object Expression {
  def bindParams(expr: Expression, params: Map[String, Literal]): Interpreted[Expression] = {
    def eval(subExpr: Expression): Interpreted[Expression] = subExpr match {
      case v: Variable =>
        params.get(v.name) match {
          case Some(literal) if literal.asInstanceOf[Expression].kind == subExpr.kind => Valid(literal.asInstanceOf[Expression])
          case Some(literal) => Invalid(NEL.of(DivergingTypes(literal.asInstanceOf[Expression].kind.toString, List(subExpr.kind.toString))))
          case None => Invalid(NEL.of(NoVariableBinding(v, params)))
        }
      case _ =>
        subExpr.children.traverse(eval(_)).map(subExpr.withChildren)
    }
    eval(expr)
  }
}

/** The ur-type for a recursive representation of MapAlgebra operations */
sealed abstract class Expression(val sym: String) extends Product with Serializable {
  def symbol: String = sym
  def children: List[Expression]
  def kind: MamlKind
  def withChildren(newChildren: List[Expression]): Expression

  def bind(args: Map[String, Literal]): Interpreted[Expression] =
    children.traverse(_.bind(args)).map(this.withChildren)
}

case class Addition(children: List[Expression]) extends Expression("+") with FoldableExpression {
  val kindDerivation = FoldableExpression.imageOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Subtraction(children: List[Expression]) extends Expression("-") with FoldableExpression {
  val kindDerivation = FoldableExpression.imageOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Multiplication(children: List[Expression]) extends Expression("*") with FoldableExpression {
  val kindDerivation = FoldableExpression.imageOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Division(children: List[Expression]) extends Expression("/") with FoldableExpression {
  val kindDerivation = FoldableExpression.imageOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Max(children: List[Expression]) extends Expression("max") with FoldableExpression {
  val kindDerivation = FoldableExpression.imageOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Min(children: List[Expression]) extends Expression("min") with FoldableExpression {
  val kindDerivation = FoldableExpression.imageOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}


case class Masking(children: List[Expression]) extends Expression("mask") with BinaryExpression {
  val kindDerivation = { (k1: MamlKind, k2: MamlKind) =>
    (k1, k2) match {
      case (MamlKind.Image, MamlKind.Geom) => MamlKind.Image
      case (MamlKind.Geom, MamlKind.Image) => MamlKind.Image
      case (x1, x2) => throw new InvalidParameterException(s"Expected image and geometry kinds. Found $x1 and $x2")
    }
  }

  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

/**
 * It is a [[FoldableExpression]] though expects only 3 rasters to be passed.
 * It would use the first band of every raster and combine them into a single RGB raster.
 *
 * redBand - the nam / number of the band from the first (red) argument
 * greenBand - the name / number of the band from the second (green) argument
 * blueBand - the name / number of the band from the third (blue) argument
 */
case class RGB(children: List[Expression], redBand: String = "0", blueBand: String = "0", greenBand: String = "0") extends Expression("rgb") with FoldableExpression {
  val kindDerivation = FoldableExpression.imageOrScalarDerivation(this)(_, _)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Pow(children: List[Expression]) extends Expression("**") with BinaryExpression {
  val kindDerivation = { (k1: MamlKind, k2: MamlKind) =>
    (k1, k2) match {
      case (MamlKind.Image, MamlKind.Image) => MamlKind.Image
      case (MamlKind.Image, MamlKind.Int) => MamlKind.Image
      case (MamlKind.Int, MamlKind.Image) => MamlKind.Image
      case (MamlKind.Image, MamlKind.Double) => MamlKind.Image
      case (MamlKind.Double, MamlKind.Image) => MamlKind.Image
      case (MamlKind.Double, MamlKind.Int) => MamlKind.Double
      case (MamlKind.Int, MamlKind.Double) => MamlKind.Double
      case (MamlKind.Int, MamlKind.Int) => MamlKind.Double
      case (MamlKind.Double, MamlKind.Double) => MamlKind.Double
      case (x1, x2) => throw new InvalidParameterException(s"Expected image or scalar kinds. Found $x1 and $x2")
    }
  }
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Lesser(children: List[Expression]) extends Expression("<") with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class LesserOrEqual(children: List[Expression]) extends Expression("<=") with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Unequal(children: List[Expression]) extends Expression("!=") with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Equal(children: List[Expression]) extends Expression("=") with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class GreaterOrEqual(children: List[Expression]) extends Expression(">=") with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Greater(children: List[Expression]) extends Expression(">") with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Or(children: List[Expression]) extends Expression("or") with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Xor(children: List[Expression]) extends Expression("xor") with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class And(children: List[Expression]) extends Expression("and") with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Atan2(children: List[Expression]) extends Expression("atan2") with BinaryExpression {
  val kindDerivation = BinaryExpression.scalarCompareDerivation _
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

/** FLOW CONTROL */
case class Branch(children: List[Expression]) extends Expression("ifelse") {
  require(children.length == 3, s"Incorrect number of arguments to a branching/if-else expression. Expected 3, found ${children.length}")
  require(children(0).kind == MamlKind.Bool, s"The first argument to branching/if-else must have Kind Bool. Found ${children(0).kind}")
  require(children(1).kind == children(2).kind, s"Unable to determine branching/if-else kind. If and Else body must be of the same kind. If-body: ${children(1).kind}. Else-body: ${children(2).kind}")
  lazy val kind = children(1).kind
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

/** Operations which should only have one argument. */
case class Classification(children: List[Expression], classMap: ClassMap) extends Expression("classify") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Sin(children: List[Expression]) extends Expression("sin") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Cos(children: List[Expression]) extends Expression("cos") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Tan(children: List[Expression]) extends Expression("tan") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Sinh(children: List[Expression]) extends Expression("sinh") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Cosh(children: List[Expression]) extends Expression("cosh") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Tanh(children: List[Expression]) extends Expression("tanh") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Asin(children: List[Expression]) extends Expression("asin") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Acos(children: List[Expression]) extends Expression("acos") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Atan(children: List[Expression]) extends Expression("atan") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Round(children: List[Expression]) extends Expression("round") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Floor(children: List[Expression]) extends Expression("floor") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Ceil(children: List[Expression]) extends Expression("ceil") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

/** Natural Log */
case class LogE(children: List[Expression]) extends Expression("loge") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Log10(children: List[Expression]) extends Expression("log10") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class SquareRoot(children: List[Expression]) extends Expression("sqrt") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Abs(children: List[Expression]) extends Expression("abs") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Defined(children: List[Expression]) extends Expression("def") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOnly
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Undefined(children: List[Expression]) extends Expression("undef") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOnly
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class NumericNegation(children: List[Expression]) extends Expression("nneg") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class LogicalNegation(children: List[Expression]) extends Expression("lneg") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOnly ++ UnaryExpression.boolOnly
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Rescale(children: List[Expression], newMin: Double, newMax: Double, band: Option[String] = None) extends Expression("rescale") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Normalize(children: List[Expression], oldMin: Double, oldMax: Double, newMin: Double, newMax: Double, band: Option[String] = None) extends Expression("normalize") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class Clamp(children: List[Expression], min: Double, max: Double, band: Option[String] = None) extends Expression("clamp") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalMax(children: List[Expression], neighborhood: Neighborhood, target: TargetCell = TargetCell.All) extends Expression("fmax") with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalMin(children: List[Expression], neighborhood: Neighborhood, target: TargetCell = TargetCell.All) extends Expression("fmin") with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalMean(children: List[Expression], neighborhood: Neighborhood, target: TargetCell = TargetCell.All) extends Expression("fmean") with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalMedian(children: List[Expression], neighborhood: Neighborhood, target: TargetCell = TargetCell.All) extends Expression("fmedian") with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalMode(children: List[Expression], neighborhood: Neighborhood, target: TargetCell = TargetCell.All) extends Expression("fmode") with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalSum(children: List[Expression], neighborhood: Neighborhood, target: TargetCell = TargetCell.All) extends Expression("fsum") with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalStdDev(children: List[Expression], neighborhood: Neighborhood, target: TargetCell = TargetCell.All) extends Expression("fstddev") with FocalExpression {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalSlope(children: List[Expression], zFactor: Option[Double] = None, target: TargetCell = TargetCell.All) extends Expression("fslope") with FocalExpression {
  // Not used in this focal operation
  def neighborhood = Square(1)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalHillshade(children: List[Expression], azimuth: Double, altitude: Double, zFactor: Option[Double] = None, target: TargetCell = TargetCell.All) extends Expression("fhillshade") with FocalExpression {
  // Not used in this focal operation
  def neighborhood = Square(1)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class FocalAspect(children: List[Expression], target: TargetCell = TargetCell.All) extends Expression("faspect") with FocalExpression {
  // Not used in this focal operation
  def neighborhood = Square(1)
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class ImageSelect(children: List[Expression], labels: List[String]) extends Expression("sel") with UnaryExpression {
  val kindDerivation: Map[MamlKind, MamlKind] = UnaryExpression.imageOnly
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}

case class IntLit(value: Int) extends Expression("int") with Literal {
  val kind = MamlKind.Int
}

case class IntVar(name: String) extends Expression("intV") with Variable {
  val kind = MamlKind.Int
}

case class DblLit(value: Double) extends Expression("dbl") with Literal {
  val kind = MamlKind.Double
}

case class DblVar(name: String) extends Expression("dblV") with Variable {
  val kind = MamlKind.Double
}

case class BoolLit(value: Boolean) extends Expression("bool") with Literal {
  val kind = MamlKind.Bool
}

case class BoolVar(name: String) extends Expression("boolV") with Variable {
  val kind = MamlKind.Bool
}

case class GeomLit(geom: String) extends Expression("geom") with Literal {
  val kind = MamlKind.Geom
}

case class GeomVar(name: String) extends Expression("geomV") with Variable {
  val kind = MamlKind.Geom
}

case class RasterLit[A](raster: A) extends Expression("raster") with Literal {
  val kind = MamlKind.Image
}

case class RasterVar(name: String) extends Expression("rasterV") with Variable {
  val kind = MamlKind.Image
}

case class Sleep(seconds: Long, children: List[Expression]) extends Expression("sleep") with UnaryExpression {
  val kindDerivation = UnaryExpression.imageOrScalar
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}
