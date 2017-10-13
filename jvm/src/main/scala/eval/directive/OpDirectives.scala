package com.azavea.maml.eval.directive

import com.azavea.maml.eval._
import com.azavea.maml.eval.tile._
import com.azavea.maml.ast._
import com.azavea.maml.dsl.tile._

import cats._
import cats.data._
import cats.implicits._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import geotrellis.vector._
import geotrellis.raster.{Tile, isData}

import scala.util.Try


object OpDirectives {
  private def doubleResults(grouped: Map[MamlKind, Seq[Result]]): Interpreted[List[Double]] =
    grouped.getOrElse(MamlKind.Scalar, List.empty).map(_.as[Double]).toList.sequence

  private def lazytileResults(grouped: Map[MamlKind, Seq[Result]]): Interpreted[List[LazyTile]] =
    grouped(MamlKind.Tile).map(_.as[LazyTile]).toList.sequence

  private def not[A, B](f: (A, B) => Boolean): (A, B) => Boolean = !f(_, _)

  private def tileOrBoolReduction(
   td: (LazyTile, Double) => LazyTile,
   dt: (Double, LazyTile) => LazyTile,
   tt: (LazyTile, LazyTile) => LazyTile,
   dd: (Double, Double) => Boolean,
   res1: Result,
   res2: Result
  ): Result = (res1, res2) match {
   case (TileResult(lt1), TileResult(lt2)) => TileResult(tt(lt1, lt2))
   case (TileResult(lt), ScalarResult(double)) => TileResult(td(lt, double))
   case (ScalarResult(double), TileResult(lt)) => TileResult(dt(double, lt))
   case (ScalarResult(dbl1), ScalarResult(dbl2)) => BoolResult(dd(dbl1, dbl2))
  }

  private def tileOrScalarReduction(
   td: (LazyTile, Double) => LazyTile,
   dt: (Double, LazyTile) => LazyTile,
   tt: (LazyTile, LazyTile) => LazyTile,
   dd: (Double, Double) => Double,
   res1: Result,
   res2: Result
  ): Result = (res1, res2) match {
   case (TileResult(lt1), TileResult(lt2)) => TileResult(tt(lt1, lt2))
   case (TileResult(lt), ScalarResult(double)) => TileResult(td(lt, double))
   case (ScalarResult(double), TileResult(lt)) => TileResult(dt(double, lt))
   case (ScalarResult(dbl1), ScalarResult(dbl2)) => ScalarResult(dd(dbl1, dbl2))
  }

  /** Arithmetic Operations */
  val additionScalar = Directive { case (a@Addition(_), childResults) if (a.kind == MamlKind.Scalar) =>
    childResults
      .map({ _.as[Double] })
      .toList.sequence
      .andThen({ results => Valid(ScalarResult(results.reduce(_ + _))) })
  }

  val additionTile = Directive { case (a@Addition(_), childResults) if (a.kind == MamlKind.Tile) =>
    val grouped = childResults.groupBy(_.kind)

    val scalarSums =
      doubleResults(grouped).map { case dbls => dbls.sum }

    (lazytileResults(grouped) |@| scalarSums).map { case (tiles, sums) =>
      val tileSum = tiles.reduce({ (lt1: LazyTile, lt2: LazyTile) => LazyTile.DualCombine(List(lt1, lt2), {_ + _}, {_ + _}) })
      TileResult(LazyTile.DualMap(List(tileSum), { i: Int => i + sums.toInt }, { i: Double => i + sums }))
    }
  }

  val subtraction = Directive { case (a@Subtraction(_), childResults) =>
    val results = childResults.reduce({ (res1, res2) =>
      tileOrScalarReduction(
        {_ - _}, {_ -: _}, {_ - _},
        {_ - _},
        res1, res2)
    })
    Valid(results)
  }

  val division = Directive { case (a@Division(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrScalarReduction(
        {_ / _}, {_ /: _}, {_ / _},
        {_ / _},
        res1, res2)
    })
    Valid(results)
  }

  val multiplicationScalar = Directive { case (a@Multiplication(_), childResults) if (a.kind == MamlKind.Scalar) =>
    childResults
      .map({ _.as[Double] })
      .toList.sequence
      .andThen({ results => Valid(ScalarResult(results.reduce(_ * _))) })
  }

  val multiplicationTile = Directive { case (a@Multiplication(_), childResults) if (a.kind == MamlKind.Tile) =>
    val grouped = childResults.groupBy(_.kind)

    val scalarProduct =
      doubleResults(grouped).map { case dbls => dbls.product}

    (lazytileResults(grouped) |@| scalarProduct).map { case (tiles, product) =>
      val tileProduct = tiles.reduce({ (lt1: LazyTile, lt2: LazyTile) => LazyTile.DualCombine(List(lt1, lt2), {_ * _}, {_ * _}) })
      TileResult(LazyTile.DualMap(List(tileProduct), { i: Int => i * product.toInt }, { i: Double => i * product }))
    }
  }

  val pow = Directive { case (p@Pow(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrScalarReduction(
        {_ ** _}, {_ **: _}, {_ ** _},
        { math.pow(_, _).toInt },
        res1, res2
      )
    })
    Valid(results)
  }

  /** Numeric Comparison Operations */
  val maxScalar = Directive { case (a@Max(_), childResults) if (a.kind == MamlKind.Scalar) =>
    childResults
      .map({ _.as[Double] })
      .toList.sequence
      .andThen({ results => Valid(ScalarResult(results.reduce(_ max _))) })
  }

  val maxTile = Directive { case (a@Max(_), childResults) if (a.kind == MamlKind.Tile) =>
  val grouped = childResults.groupBy(_.kind)

  val scalarMax: Interpreted[Option[Double]] =
    doubleResults(grouped).map({ case dbls => Try(dbls.max).toOption })

  (lazytileResults(grouped) |@| scalarMax).map({ case (tiles, maximum) =>
    val tileMax = tiles.reduce({ (lt1: LazyTile, lt2: LazyTile) => LazyTile.DualCombine(List(lt1, lt2), {_ max _}, {_ max _}) })
    maximum match {
      case Some(scalarMax) =>
        TileResult(LazyTile.DualMap(List(tileMax), { i: Int => i max scalarMax.toInt }, { i: Double => i max scalarMax }))
      case None =>
        TileResult(tileMax)
    }
  })
  }

  val minScalar = Directive { case (a@Min(_), childResults) if (a.kind == MamlKind.Scalar) =>
  childResults
    .map({ _.as[Double] })
    .toList.sequence
    .andThen({ results => Valid(ScalarResult(results.reduce(_ min _))) })
  }

  val minTile = Directive { case (a@Min(_), childResults) if (a.kind == MamlKind.Tile) =>
  val grouped = childResults.groupBy(_.kind)

  val scalarMin: Interpreted[Option[Double]] =
    doubleResults(grouped).map({ case dbls => Try(dbls.min).toOption })

  (lazytileResults(grouped) |@| scalarMin).map({ case (tiles, minimum) =>
    val tileMin = tiles.reduce({ (lt1: LazyTile, lt2: LazyTile) => LazyTile.DualCombine(List(lt1, lt2), {_ min _}, {_ min _}) })
    minimum match {
      case Some(scalarMin) =>
        TileResult(LazyTile.DualMap(List(tileMin), { i: Int => i min scalarMin.toInt }, { i: Double => i min scalarMin }))
      case None =>
        TileResult(tileMin)
    }
  })
  }

  /** Numeric Comparison Operations */
  val lessThan = Directive { case (a@Less(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrBoolReduction(
        {_ < _}, { (d, t) => t < d }, {_ < _},
        {_ < _},
        res1, res2
      )
    })
    Valid(results)
  }

  val lessThanOrEqualTo = Directive { case (a@LessOrEqual(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrBoolReduction(
        {_ <= _}, { (d, t) => t <= d }, {_ <= _},
        {_ <= _},
        res1, res2
      )
    })
    Valid(results)
  }

  val equalTo = Directive { case (a@Equal(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrBoolReduction(
        {_ === _}, { (d, t) => t === d }, {_ === _},
        {_ == _},
        res1, res2
      )
    })
    Valid(results)
  }

  val notEqualTo = Directive { case (a@Unequal(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrBoolReduction(
        {_ !== _}, { (d, t) => t !== d }, {_ !== _},
        {_ != _},
        res1, res2
      )
    })
    Valid(results)
  }

  val greaterThan = Directive { case (a@Greater(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrBoolReduction(
        {_ > _}, { (d, t) => t > d }, {_ > _},
        {_ > _},
        res1, res2
      )
    })
    Valid(results)
  }

  val greaterThanOrEqualTo = Directive { case (a@GreaterOrEqual(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrBoolReduction(
        {_ >= _}, { (d, t) => t > d }, {_ >= _},
        {_ >= _},
        res1, res2
      )
    })
    Valid(results)
  }

  /** Logical Operations */
  // TODO: Update these functions when the int/double distinction is removed so that bool args
  //        are respected
  val and = Directive { case (and@And(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrBoolReduction(
        {_ && _}, { _ &&: _ }, {_ && _},
        {isData(_) && isData(_)},
        res1, res2
      )
    })
    Valid(results)
  }

  val or = Directive { case (or@Or(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrBoolReduction(
        {_ || _}, { _ ||: _ }, {_ || _},
        {isData(_) || isData(_)},
        res1, res2
      )
    })
    Valid(results)
  }

  val xor = Directive { case (xor@Xor(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrBoolReduction(
        {_ xor _}, { (d, t) => t xor d }, {_ xor _},
        {(x, y) => (isData(x) || isData(y) && !(isData(x) && isData(y)))},
        res1, res2
      )
    })
    Valid(results)
  }

  /** Tile-specific Operations */
  val masking = Directive { case (mask@Masking(_), childResults) =>
    ((childResults(0), childResults(1)) match {
      case (TileResult(lzTile), GeomResult(geom)) =>
        Valid((lzTile, geom))
      case (GeomResult(geom), TileResult(lzTile)) =>
        Valid((lzTile, geom))
      case _ =>
        Invalid(NEL.of(NonEvaluableNode(mask, Some("Masking operation requires both a tile and a vector argument"))))
    }).andThen({ case (lzTile, geom) =>
      geom.as[MultiPolygon] match {
        case Some(mp) =>
          Valid(TileResult(MaskingNode(List(lzTile), mp)))
        case None =>
          Invalid(NEL.of(NonEvaluableNode(mask, Some("Masking operation requires its vector argument to be a multipolygon"))))
      }
    })
  }

  /** Trigonometric Operations */
  val atan2 = Directive { case (atan2@Atan2(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrScalarReduction(
        {_.atan2(_)}, { (d, t) => t.atan2(d) }, {_.atan2(_)},
        { math.atan2(_, _).toInt },
        res1, res2
      )
    })
    Valid(results)
  }
}

