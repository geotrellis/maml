package com.azavea.maml.eval

import com.azavea.maml.error._
import com.azavea.maml.ast._
import com.azavea.maml.util._
import com.azavea.maml.eval.directive._
import com.azavea.maml.eval.tile._

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}
import geotrellis.raster.{Tile, Raster, GridBounds}
import geotrellis.raster.mapalgebra.focal

import scala.reflect.ClassTag


case class BufferingInterpreter(
  directives: List[ScopedDirective[BufferingInterpreter.Scope]],
  options: BufferingInterpreter.Options = BufferingInterpreter.Options(256)
) extends ScopedInterpreter[BufferingInterpreter.Scope] {

  def prependDirective(directive: ScopedDirective[BufferingInterpreter.Scope]): ScopedInterpreter[BufferingInterpreter.Scope] =
    BufferingInterpreter(directive +: directives, options)

  def appendDirective(directive: ScopedDirective[BufferingInterpreter.Scope]): ScopedInterpreter[BufferingInterpreter.Scope] =
    BufferingInterpreter(directives :+ directive, options)

  def scopeFor(exp: Expression, previous: Option[BufferingInterpreter.Scope]): BufferingInterpreter.Scope = {
    val scope = previous.getOrElse(BufferingInterpreter.Scope(0, options.tileSize))
    exp match {
      case f: FocalExpression =>
        val n = NeighborhoodConversion(f.neighborhood)
        scope.copy(buffer = scope.buffer + n.extent)
      case _ => scope
    }
  }

  val fallbackDirective: ScopedDirective[BufferingInterpreter.Scope] =
    { case (exp, res, scope) => Invalid(NEL.of(UnhandledCase(exp, exp.kind))) }

  def instructions(expression: Expression, children: Seq[Result], scope: BufferingInterpreter.Scope): Interpreted[Result] =
    directives.reduceLeft(_ orElse _).orElse(fallbackDirective)((expression, children, scope))
}


object BufferingInterpreter {
  case class Scope(buffer: Int, tileSize: Int)
  case class Options(tileSize: Int)

  def DEFAULT = BufferingInterpreter(
    List(
      ScopedDirective.pure[RasterLit[_]](SourceDirectives.rasterLiteral),
      ScopedDirective.pure[IntLit](SourceDirectives.intLiteral),
      ScopedDirective.pure[DblLit](SourceDirectives.dblLiteral),
      ScopedDirective.pure[BoolLit](SourceDirectives.boolLiteral),
      ScopedDirective.pure[GeomLit](SourceDirectives.geoJson),
      ScopedDirective.pure[Addition](OpDirectives.additionTile orElse OpDirectives.additionInt orElse OpDirectives.additionDouble),
      ScopedDirective.pure[Subtraction](OpDirectives.subtraction),
      ScopedDirective.pure[Multiplication](OpDirectives.multiplicationTile orElse OpDirectives.multiplicationInt orElse OpDirectives.multiplicationDouble),
      ScopedDirective.pure[Division](OpDirectives.division),
      ScopedDirective.pure[Pow](OpDirectives.pow),
      ScopedDirective.pure[Max](OpDirectives.maxTile orElse OpDirectives.maxInt orElse OpDirectives.maxDouble),
      ScopedDirective.pure[Min](OpDirectives.minTile orElse OpDirectives.minInt orElse OpDirectives.minDouble),
      ScopedDirective.pure[Lesser](OpDirectives.lessThan),
      ScopedDirective.pure[LesserOrEqual](OpDirectives.lessThanOrEqualTo),
      ScopedDirective.pure[Equal](OpDirectives.equalTo),
      ScopedDirective.pure[Unequal](OpDirectives.notEqualTo),
      ScopedDirective.pure[Greater](OpDirectives.greaterThan),
      ScopedDirective.pure[GreaterOrEqual](OpDirectives.greaterThanOrEqualTo),
      ScopedDirective.pure[And](OpDirectives.and),
      ScopedDirective.pure[Or](OpDirectives.or),
      ScopedDirective.pure[Xor](OpDirectives.xor),
      ScopedDirective.pure[Masking](OpDirectives.masking),
      ScopedDirective.pure[Atan2](OpDirectives.atan2),
      ScopedDirective.pure[Sin](UnaryDirectives.sin),
      ScopedDirective.pure[Cos](UnaryDirectives.cos),
      ScopedDirective.pure[Tan](UnaryDirectives.tan),
      ScopedDirective.pure[Sinh](UnaryDirectives.sinh),
      ScopedDirective.pure[Cosh](UnaryDirectives.cosh),
      ScopedDirective.pure[Tanh](UnaryDirectives.tanh),
      ScopedDirective.pure[Asin](UnaryDirectives.asin),
      ScopedDirective.pure[Acos](UnaryDirectives.acos),
      ScopedDirective.pure[Atan](UnaryDirectives.atan),
      ScopedDirective.pure[Round](UnaryDirectives.round),
      ScopedDirective.pure[Floor](UnaryDirectives.floor),
      ScopedDirective.pure[Ceil](UnaryDirectives.ceil),
      ScopedDirective.pure[LogE](UnaryDirectives.naturalLog),
      ScopedDirective.pure[Log10](UnaryDirectives.log10),
      ScopedDirective.pure[SquareRoot](UnaryDirectives.sqrt),
      ScopedDirective.pure[Abs](UnaryDirectives.abs),
      ScopedDirective.pure[Undefined](UnaryDirectives.isUndefined),
      ScopedDirective.pure[Defined](UnaryDirectives.isDefined),
      ScopedDirective.pure[NumericNegation](UnaryDirectives.numericNegation),
      ScopedDirective.pure[LogicalNegation](UnaryDirectives.logicalNegation),
      ScopedDirective.pure[Classification](UnaryDirectives.classification),
      ScopedDirective.pure[ImageSelect](UnaryDirectives.imageSelection),
      focalMax,
      focalMin,
      focalMean,
      focalMode,
      focalMedian,
      focalSum,
      focalStandardDeviation,
      ScopedDirective.pure[FocalSlope](FocalDirectives.slope),
      ScopedDirective.pure[FocalHillshade](FocalDirectives.hillshade)
    ), Options(256)
  )

  val focalMax = ScopedDirective[Scope] { case (fm@FocalMax(_, neighborhood), childResults, scope) =>
    val n = NeighborhoodConversion(neighborhood)
    val gridbounds =
      GridBounds(n.extent, n.extent, scope.tileSize - 1 + scope.buffer * 2 + n.extent, scope.tileSize - 1 + scope.buffer * 2 + n.extent)

    childResults.head match {
      case ImageResult(lzTile) =>
        Valid(ImageResult(lzTile.focal(n, Some(gridbounds), focal.Max.apply _)))
    }
  }

  val focalMin = ScopedDirective[Scope] { case (fm@FocalMin(_, neighborhood), childResults, scope) =>
    val n = NeighborhoodConversion(neighborhood)
    val gridbounds =
      GridBounds(n.extent, n.extent, scope.tileSize - 1 + scope.buffer * 2 + n.extent, scope.tileSize - 1 + scope.buffer * 2 + n.extent)

    childResults.head match {
      case ImageResult(lzTile) =>
        Valid(ImageResult(lzTile.focal(n, Some(gridbounds), focal.Min.apply _)))
    }
  }

  val focalMean = ScopedDirective[Scope] { case (fm@FocalMean(_, neighborhood), childResults, scope) =>
    val n = NeighborhoodConversion(neighborhood)
    val gridbounds =
      GridBounds(n.extent, n.extent, scope.tileSize - 1 + scope.buffer * 2 + n.extent, scope.tileSize - 1 + scope.buffer * 2 + n.extent)

    childResults.head match {
      case ImageResult(lzTile) =>
        Valid(ImageResult(lzTile.focal(n, Some(gridbounds), focal.Mean.apply _)))
    }
  }

  val focalMedian = ScopedDirective[Scope] { case (fm@FocalMedian(_, neighborhood), childResults, scope) =>
    val n = NeighborhoodConversion(neighborhood)
    val gridbounds =
      GridBounds(n.extent, n.extent, scope.tileSize - 1 + scope.buffer * 2 + n.extent, scope.tileSize - 1 + scope.buffer * 2 + n.extent)

    childResults.head match {
      case ImageResult(lzTile) =>
        Valid(ImageResult(lzTile.focal(n, Some(gridbounds), focal.Median.apply _)))
    }
  }

  val focalMode = ScopedDirective[Scope] { case (fm@FocalMode(_, neighborhood), childResults, scope) =>
    val n = NeighborhoodConversion(neighborhood)
    val gridbounds =
      GridBounds(n.extent, n.extent, scope.tileSize - 1 + scope.buffer * 2 + n.extent, scope.tileSize - 1 + scope.buffer * 2 + n.extent)

    childResults.head match {
      case ImageResult(lzTile) =>
        Valid(ImageResult(lzTile.focal(n, Some(gridbounds), focal.Mode.apply _)))
    }
  }

  val focalSum = ScopedDirective[Scope] { case (fm@FocalSum(_, neighborhood), childResults, scope) =>
    val n = NeighborhoodConversion(neighborhood)
    val gridbounds =
      GridBounds(n.extent, n.extent, scope.tileSize - 1 + scope.buffer * 2 + n.extent, scope.tileSize - 1 + scope.buffer * 2 + n.extent)

    childResults.head match {
      case ImageResult(lzTile) =>
        Valid(ImageResult(lzTile.focal(n, Some(gridbounds), focal.Sum.apply _)))
    }
  }

  val focalStandardDeviation = ScopedDirective[Scope] { case (fm@FocalStdDev(_, neighborhood), childResults, scope) =>
    val n = NeighborhoodConversion(neighborhood)
    val gridbounds =
      GridBounds(n.extent, n.extent, scope.tileSize - 1 + scope.buffer * 2 + n.extent, scope.tileSize - 1 + scope.buffer * 2 + n.extent)

    childResults.head match {
      case ImageResult(lzTile) =>
        Valid(ImageResult(lzTile.focal(n, Some(gridbounds), focal.StandardDeviation.apply _)))
    }
  }
}
