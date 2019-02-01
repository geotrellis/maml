package com.azavea.maml.eval.directive

import com.azavea.maml.error._
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
    grouped.getOrElse(MamlKind.Double, List.empty).map(_.as[Double]).toList.sequence

  private def intResults(grouped: Map[MamlKind, Seq[Result]]): Interpreted[List[Int]] =
    grouped.getOrElse(MamlKind.Int, List.empty).map(_.as[Int]).toList.sequence

  private def imageResults(grouped: Map[MamlKind, Seq[Result]]): Interpreted[List[LazyMultibandRaster]] =
    grouped(MamlKind.Image).map(_.as[LazyMultibandRaster]).toList.sequence

  private def not[A, B](f: (A, B) => Boolean): (A, B) => Boolean = !f(_, _)

  private def tileOrBoolReduction(
   ti: (LazyMultibandRaster, Int) => LazyMultibandRaster,
   it: (Int, LazyMultibandRaster) => LazyMultibandRaster,
   td: (LazyMultibandRaster, Double) => LazyMultibandRaster,
   dt: (Double, LazyMultibandRaster) => LazyMultibandRaster,
   tt: (LazyMultibandRaster, LazyMultibandRaster) => LazyMultibandRaster,
   ii: (Int, Int) => Boolean,
   di: (Double, Int) => Boolean,
   dd: (Double, Double) => Boolean,
   id: (Int, Double) => Boolean,
   res1: Result,
   res2: Result
  ): Result = (res1, res2) match {
   case (ImageResult(lt1), ImageResult(lt2)) => ImageResult(tt(lt1, lt2))
   case (ImageResult(lt), IntResult(int)) => ImageResult((ti(lt, int)))
   case (IntResult(int), ImageResult(lt)) => ImageResult((it(int, lt)))
   case (ImageResult(lt), DoubleResult(double)) => ImageResult(td(lt, double))
   case (DoubleResult(double), ImageResult(lt)) => ImageResult(dt(double, lt))
   case (IntResult(int1), IntResult(int2)) => BoolResult(ii(int1, int2))
   case (DoubleResult(dbl), IntResult(int)) => BoolResult(di(dbl, int))
   case (DoubleResult(dbl1), DoubleResult(dbl2)) => BoolResult(dd(dbl1, dbl2))
   case (IntResult(int), DoubleResult(dbl)) => BoolResult(id(int, dbl))
  }

  private def tileOrScalarReduction(
   ti: (LazyMultibandRaster, Int) => LazyMultibandRaster,
   it: (Int, LazyMultibandRaster) => LazyMultibandRaster,
   td: (LazyMultibandRaster, Double) => LazyMultibandRaster,
   dt: (Double, LazyMultibandRaster) => LazyMultibandRaster,
   tt: (LazyMultibandRaster, LazyMultibandRaster) => LazyMultibandRaster,
   ii: (Int, Int) => Int,
   di: (Double, Int) => Double,
   dd: (Double, Double) => Double,
   id: (Int, Double) => Double,
   res1: Result,
   res2: Result
  ): Result = (res1, res2) match {
   case (ImageResult(lt1), ImageResult(lt2)) => ImageResult(tt(lt1, lt2))
   case (ImageResult(lt), IntResult(int)) => ImageResult((ti(lt, int)))
   case (IntResult(int), ImageResult(lt)) => ImageResult((it(int, lt)))
   case (ImageResult(lt), DoubleResult(double)) => ImageResult(td(lt, double))
   case (DoubleResult(double), ImageResult(lt)) => ImageResult(dt(double, lt))
   case (IntResult(int1), IntResult(int2)) => IntResult(ii(int1, int2))
   case (DoubleResult(dbl), IntResult(int)) => DoubleResult(di(dbl, int))
   case (DoubleResult(dbl1), DoubleResult(dbl2)) => DoubleResult(dd(dbl1, dbl2))
   case (IntResult(int), DoubleResult(dbl)) => DoubleResult(id(int, dbl))
  }

  /** Arithmetic Operations */
  val additionDouble = Directive { case (a@Addition(_), childResults) if (a.kind == MamlKind.Double) =>
    childResults
      .map({ _.as[Double] })
      .toList.sequence
      .andThen({ results => Valid(DoubleResult(results.reduce(_ + _))) })
  }

  val additionInt = Directive { case (a@Addition(_), childResults) if (a.kind == MamlKind.Int) =>
    childResults
      .map({ _.as[Int] })
      .toList.sequence
      .andThen({ results => Valid(IntResult(results.reduce(_ + _))) })
  }

  val additionTile = Directive { case (a@Addition(_), childResults) if (a.kind == MamlKind.Image) =>
    val grouped = childResults.groupBy(_.kind)

    val scalarSums =
      (doubleResults(grouped), intResults(grouped)).mapN { case (dbls, ints) => dbls.sum + ints.sum }

    (imageResults(grouped), scalarSums).mapN { case (tiles, sums) =>
      val tileSum = tiles.reduce({ (lt1: LazyMultibandRaster, lt2: LazyMultibandRaster) => lt1.dualCombine(lt2, {_ + _}, {_ + _}) })
      ImageResult(tileSum.dualMap({ i: Int => i + sums.toInt }, { i: Double => i + sums }))
    }
  }

  val subtraction = Directive { case (a@Subtraction(_), childResults) =>
    val results = childResults.reduce({ (res1, res2) =>
      tileOrScalarReduction(
        {_ - _}, {_ -: _}, {_ - _}, {_ -: _}, {_ - _},
        {_ - _}, {_ - _}, {_ - _}, {_ - _},
        res1, res2)
    })
    Valid(results)
  }

  val division = Directive { case (a@Division(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrScalarReduction(
        {_ / _}, {_ /: _}, {_ / _}, {_ /: _}, {_ / _},
        {_ / _}, {_ / _}, {_ / _}, {_ / _},
        res1, res2)
    })
    Valid(results)
  }

  val multiplicationDouble = Directive { case (a@Multiplication(_), childResults) if (a.kind == MamlKind.Double) =>
    childResults
      .map({ _.as[Double] })
      .toList.sequence
      .andThen({ results => Valid(DoubleResult(results.reduce(_ * _))) })
  }

  val multiplicationInt = Directive { case (a@Multiplication(_), childResults) if (a.kind == MamlKind.Double) =>
    childResults
      .map({ _.as[Double] })
      .toList.sequence
      .andThen({ results => Valid(DoubleResult(results.reduce(_ * _))) })
  }

  val multiplicationTile = Directive { case (a@Multiplication(_), childResults) if (a.kind == MamlKind.Image) =>
    val grouped = childResults.groupBy(_.kind)

    val scalarProduct =
      (doubleResults(grouped), intResults(grouped)).mapN { case (dbls, ints) => dbls.product * ints.product }

    (imageResults(grouped), scalarProduct).mapN { case (tiles, product) =>
      val tileProduct = tiles.reduce({ (lt1: LazyMultibandRaster, lt2: LazyMultibandRaster) => lt1.dualCombine(lt2, {_ * _}, {_ * _}) })
      ImageResult(tileProduct.dualMap({ i: Int => i * product.toInt }, { i: Double => i * product }))
    }
  }

  val pow = Directive { case (p@Pow(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrScalarReduction(
        {_ ** _}, {_ **: _}, {_ ** _}, {_ **: _}, {_ ** _},
        { math.pow(_, _).toInt }, { math.pow(_, _) }, { math.pow(_, _) }, { math.pow(_, _) },
        res1, res2
      )
    })
    Valid(results)
  }

  /** Numeric Comparison Operations */
  val maxDouble = Directive { case (a@Max(_), childResults) if (a.kind == MamlKind.Double) =>
    childResults
      .map({ _.as[Double] })
      .toList.sequence
      .andThen({ results => Valid(DoubleResult(results.reduce(_ max _))) })
  }

  val maxInt = Directive { case (a@Max(_), childResults) if (a.kind == MamlKind.Double) =>
    childResults
      .map({ _.as[Double] })
      .toList.sequence
      .andThen({ results => Valid(DoubleResult(results.reduce(_ max _))) })
  }

  val maxTile = Directive { case (a@Max(_), childResults) if (a.kind == MamlKind.Image) =>
  val grouped = childResults.groupBy(_.kind)

  val scalarMax: Interpreted[Option[Double]] =
    (doubleResults(grouped), intResults(grouped)).mapN { case (dbls, ints) =>
      (Try(dbls.max).toOption, Try(ints.max).toOption) match {
        case (Some(dbl), Some(int)) => Some(dbl max int)
        case (None, Some(int)) => Some(int)
        case (Some(dbl), None) => Some(dbl)
        case _ => None
      }
    }

  (imageResults(grouped), scalarMax).mapN({ case (tiles, maximum) =>
    val tileMax = tiles.reduce({ (lt1: LazyMultibandRaster, lt2: LazyMultibandRaster) =>
      lt1.dualCombine(lt2, {_ max _}, {_ max _})
    })
    maximum match {
      case Some(scalarMax) =>
        ImageResult(tileMax.dualMap({ i: Int => i max scalarMax.toInt }, { i: Double => i max scalarMax }))
      case None =>
        ImageResult(tileMax)
    }
  })
  }

  val minDouble = Directive { case (a@Min(_), childResults) if (a.kind == MamlKind.Double) =>
  childResults
    .map({ _.as[Double] })
    .toList.sequence
    .andThen({ results => Valid(DoubleResult(results.reduce(_ min _))) })
  }

  val minInt = Directive { case (a@Min(_), childResults) if (a.kind == MamlKind.Double) =>
  childResults
    .map({ _.as[Double] })
    .toList.sequence
    .andThen({ results => Valid(DoubleResult(results.reduce(_ min _))) })
  }

  val minTile = Directive { case (a@Min(_), childResults) if (a.kind == MamlKind.Image) =>
  val grouped = childResults.groupBy(_.kind)

  val scalarMin: Interpreted[Option[Double]] =
    (doubleResults(grouped), intResults(grouped)).mapN { case (dbls, ints) =>
      (Try(dbls.min).toOption, Try(ints.min).toOption) match {
        case (Some(dbl), Some(int)) => Some(dbl min int)
        case (None, Some(int)) => Some(int)
        case (Some(dbl), None) => Some(dbl)
        case _ => None
      }
    }

  (imageResults(grouped), scalarMin).mapN({ case (tiles, minimum) =>
    val tileMin = tiles.reduce({ (lt1: LazyMultibandRaster, lt2: LazyMultibandRaster) =>
      lt1.dualCombine(lt2, {_ min _}, {_ min _})
    })
    minimum match {
      case Some(scalarMin) =>
        ImageResult(tileMin.dualMap({ i: Int => i min scalarMin.toInt }, { i: Double => i min scalarMin }))
      case None =>
        ImageResult(tileMin)
    }
  })
  }

  /** Numeric Comparison Operations */
  val lessThan = Directive { case (a@Lesser(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrBoolReduction(
        {_ < _}, { (i, t) => t < i }, {_ < _}, { (d, t) => t < d }, {_ < _},
        {_ < _}, {_ < _}, {_ < _}, {_ < _.toInt},
        res1, res2
      )
    })
    Valid(results)
  }

  val lessThanOrEqualTo = Directive { case (a@LesserOrEqual(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrBoolReduction(
        {_ <= _}, { (i, t) => t <= i }, {_ <= _}, { (d, t) => t <= d }, {_ <= _},
        {_ <= _}, {_ <= _}, {_ <= _}, {_ <= _.toInt},
        res1, res2
      )
    })
    Valid(results)
  }

  val equalTo = Directive { case (a@Equal(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrBoolReduction(
        {_ === _}, { (i, t) => t === i }, {_ === _}, { (d, t) => t === d }, {_ === _},
        {_ == _}, {_ == _}, {_ == _}, {_ == _.toInt},
        res1, res2
      )
    })
    Valid(results)
  }

  val notEqualTo = Directive { case (a@Unequal(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrBoolReduction(
        {_ !== _}, { (i, t) => t !== i }, {_ !== _}, { (d, t) => t !== d }, {_ !== _},
        {_ != _}, {_ != _}, {_ != _}, {_ != _.toInt},
        res1, res2
      )
    })
    Valid(results)
  }

  val greaterThan = Directive { case (a@Greater(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrBoolReduction(
        {_ > _}, { (i, t) => t > i }, {_ > _}, { (d, t) => t > d }, {_ > _},
        {_ > _}, {_ > _}, {_ > _}, {_ > _.toInt},
        res1, res2
      )
    })
    Valid(results)
  }

  val greaterThanOrEqualTo = Directive { case (a@GreaterOrEqual(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrBoolReduction(
        {_ >= _}, { (i, t) => t > i }, {_ >= _}, { (d, t) => t > d }, {_ >= _},
        {_ >= _}, {_ >= _}, {_ >= _}, {_ >= _.toInt},
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
        {_ && _}, {_ &&: _}, {_ && _}, { _ &&: _ }, {_ && _},
        {isData(_) && isData(_)}, {isData(_) && isData(_)}, {isData(_) && isData(_)}, {isData(_) && isData(_)},
        res1, res2
      )
    })
    Valid(results)
  }

  val or = Directive { case (or@Or(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrBoolReduction(
        {_ || _}, {_ ||: _}, {_ || _}, { _ ||: _ }, {_ || _},
        {isData(_) || isData(_)}, {isData(_) || isData(_)}, {isData(_) || isData(_)}, {isData(_) || isData(_)},
        res1, res2
      )
    })
    Valid(results)
  }

  val xor = Directive { case (xor@Xor(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrBoolReduction(
        {_ xor _}, { (i, t) => t xor i}, {_ xor _}, { (d, t) => t xor d }, {_ xor _},
        {(x, y) => (isData(x) || isData(y) && !(isData(x) && isData(y)))},
        {(x, y) => (isData(x) || isData(y) && !(isData(x) && isData(y)))},
        {(x, y) => (isData(x) || isData(y) && !(isData(x) && isData(y)))},
        {(x, y) => (isData(x) || isData(y) && !(isData(x) && isData(y)))},
        res1, res2
      )
    })
    Valid(results)
  }

  /** Tile-specific Operations */
  val masking = Directive { case (mask@Masking(_), childResults) =>
    ((childResults(0), childResults(1)) match {
      case (ImageResult(lzRaster), GeomResult(geom)) =>
        Valid((lzRaster, geom))
      case (GeomResult(geom), ImageResult(lzRaster)) =>
        Valid((lzRaster, geom))
      case _ =>
        Invalid(NEL.of(NonEvaluableNode(mask, Some("Masking operation requires both a tile and a vector argument"))))
    }).andThen({ case (lzRaster, geom) =>
      val f: LazyMultibandRaster => LazyTile = ???
      geom.as[MultiPolygon] match {
        case Some(mp) =>
          Valid(ImageResult(LazyMultibandRaster(List(MaskingNode(List(f(lzRaster)), mp)))))
        case None =>
          Invalid(NEL.of(NonEvaluableNode(mask, Some("Masking operation requires its vector argument to be a multipolygon"))))
      }
    })
  }

  /** Trigonometric Operations */
  val atan2 = Directive { case (atan2@Atan2(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileOrScalarReduction(
        {_.atan2(_)}, { (i, t) => t.atan2(i) }, {_.atan2(_)}, { (d, t) => t.atan2(d) }, {_.atan2(_)},
        { math.atan2(_, _).toInt }, { math.atan2(_, _) }, { math.atan2(_, _) }, { math.atan2(_, _) },
        res1, res2
      )
    })
    Valid(results)
  }
}

