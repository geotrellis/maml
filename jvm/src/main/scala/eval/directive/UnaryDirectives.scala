package com.azavea.maml.eval.directive

import com.azavea.maml.eval._
import com.azavea.maml.eval.tile._
import com.azavea.maml.ast._
import com.azavea.maml.dsl.tile._

import cats._
import cats.implicits._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import geotrellis.raster._
import geotrellis.raster.render._

import scala.util.Try


object UnaryDirectives {

  private def not[A](f: A => Boolean): A => Boolean = !f(_)

  private def tileOrScalarResult(
    t: LazyTile => LazyTile,
    d: Double => Double,
    arg: Result
  ): Result = arg match {
    case TileResult(lt) => TileResult(t(lt))
    case ScalarResult(dbl) => ScalarResult(d(dbl))
  }

  private def tileOrBoolResult(
    t: LazyTile => LazyTile,
    d: Double => Boolean,
    arg: Result
  ): Result = arg match {
    case TileResult(lt) => TileResult(t(lt))
    case ScalarResult(dbl) => BoolResult(d(dbl))
  }

  /** Trigonometric Operations */
  val sin = Directive { case (s@Sin(_), childResults) =>
    val result = tileOrScalarResult({ _.sin }, { math.sin(_) }, childResults.head)
    Valid(result)
  }

  val cos = Directive { case (s@Cos(_), childResults) =>
    val result = tileOrScalarResult({ _.cos }, { math.cos(_) }, childResults.head)
    Valid(result)
  }

  val tan = Directive { case (s@Tan(_), childResults) =>
    val result = tileOrScalarResult({ _.tan }, { math.tan(_) }, childResults.head)
    Valid(result)
  }

  val sinh = Directive { case (s@Sinh(_), childResults) =>
    val result = tileOrScalarResult({ _.sinh }, { math.sinh(_) }, childResults.head)
    Valid(result)
  }

  val cosh = Directive { case (s@Cosh(_), childResults) =>
    val result = tileOrScalarResult({ _.cosh }, { math.cosh(_) }, childResults.head)
    Valid(result)
  }

  val tanh = Directive { case (s@Tanh(_), childResults) =>
    val result = tileOrScalarResult({ _.tanh }, { math.tanh(_) }, childResults.head)
    Valid(result)
  }

  val asin = Directive { case (s@Asin(_), childResults) =>
    val result = tileOrScalarResult({ _.asin }, { math.asin(_) }, childResults.head)
    Valid(result)
  }

  val acos = Directive { case (s@Acos(_), childResults) =>
    val result = tileOrScalarResult({ _.acos }, { math.acos(_) }, childResults.head)
    Valid(result)
  }

  val atan = Directive { case (s@Atan(_), childResults) =>
    val result = tileOrScalarResult({ _.atan }, { math.atan(_) }, childResults.head)
    Valid(result)
  }

  /** Rounding Operations */
  val round = Directive { case (r@Round(_), childResults) =>
    val result = tileOrScalarResult({ _.round }, { math.round(_) }, childResults.head)
    Valid(result)
  }
  val floor = Directive { case (r@Floor(_), childResults) =>
    val result = tileOrScalarResult({ _.floor }, { math.floor(_) }, childResults.head)
    Valid(result)
  }
  val ceil = Directive { case (r@Ceil(_), childResults) =>
    val result = tileOrScalarResult({ _.ceil }, { math.ceil(_) }, childResults.head)
    Valid(result)
  }

  /** Arithmetic Operations */
  val naturalLog = Directive { case  (nl@LogE(_), childResults) =>
    val result = tileOrScalarResult({ _.logE }, { math.log(_) }, childResults.head)
    Valid(result)
  }

  val log10 = Directive { case  (nl@Log10(_), childResults) =>
    val result = tileOrScalarResult({ _.log10 }, { math.log10(_) }, childResults.head)
    Valid(result)
  }

  val sqrt = Directive { case  (sqrt@SquareRoot(_), childResults) =>
    val result = tileOrScalarResult({ _.sqrt }, { math.sqrt(_) }, childResults.head)
    Valid(result)
  }

  val abs = Directive { case  (sqrt@SquareRoot(_), childResults) =>
    val result = tileOrScalarResult({ _.abs }, { math.abs(_) }, childResults.head)
    Valid(result)
  }

  val isDefined = Directive { case (d@Defined(_), childResults) =>
    val result = tileOrBoolResult({ _.isDefined }, { isData(_) }, childResults.head)
    Valid(result)
  }

  val isUndefined = Directive { case (d@Undefined(_), childResults) =>
    val result = tileOrBoolResult({ _.isUndefined }, { isNoData(_) }, childResults.head)
    Valid(result)
  }

  val numericNegation = Directive { case (nn@NumericNegation(_), childResults) =>
    val result = tileOrScalarResult({ _.changeSign }, {_ * -1}, childResults.head)
    Valid(result)
  }

  /** Logical Operations */
  val logicalNegation = Directive { case (ln@LogicalNegation(_), childResults) =>
    val result = tileOrBoolResult({ _.not }, {not(isData(_))}, childResults.head)
    Valid(result)
  }

  /** Tile-specific Operations */
  val classification = Directive { case (classify@Classification(_, classMap), childResults) =>
    childResults.head match {
      case TileResult(lzTile) => Valid(TileResult(lzTile.classify(BreakMap(classMap.classifications))))
      case _ => Invalid(NEL.of(NonEvaluableNode(classify, Some("Classification node requires lazytile argument"))))
    }
  }
}

