package com.azavea.maml.eval.directive

import com.azavea.maml.eval._
import com.azavea.maml.eval.tile._
import com.azavea.maml.ast._
import com.azavea.maml.dsl.tile._

import geotrellis.raster.Tile
import cats._
import cats.implicits._
import cats.data.{NonEmptyList => NEL, _}
import Validated._

import scala.util.Try


object OpDirectives {
  private def doubleResults(grouped: Map[MamlKind, Seq[Result]]): Interpreted[List[Double]] =
    grouped.getOrElse(MamlKind.Double, List.empty).map(_.as[Double]).toList.sequence

  private def intResults(grouped: Map[MamlKind, Seq[Result]]): Interpreted[List[Int]] =
    grouped.getOrElse(MamlKind.Int, List.empty).map(_.as[Int]).toList.sequence

  private def lazytileResults(grouped: Map[MamlKind, Seq[Result]]): Interpreted[List[LazyTile]] =
    grouped(MamlKind.Tile).map(_.as[LazyTile]).toList.sequence

   private def tileReduction(
     ti: (LazyTile, Int) => LazyTile,
     it: (Int, LazyTile) => LazyTile,
     td: (LazyTile, Double) => LazyTile,
     dt: (Double, LazyTile) => LazyTile,
     tt: (LazyTile, LazyTile) => LazyTile,
     res1: Result,
     res2: Result
   ): Result = (res1, res2) match {
     case (TileResult(lt1), TileResult(lt2)) => TileResult(tt(lt1, lt2))
     case (TileResult(lt), IntResult(int)) => TileResult((ti(lt, int)))
     case (IntResult(int), TileResult(lt)) => TileResult((it(int, lt)))
     case (TileResult(lt), DoubleResult(double)) => TileResult(td(lt, double))
     case (DoubleResult(double), TileResult(lt)) => TileResult(dt(double, lt))
   }

   private def tileOrBoolReduction(
     ti: (LazyTile, Int) => LazyTile,
     it: (Int, LazyTile) => LazyTile,
     td: (LazyTile, Double) => LazyTile,
     dt: (Double, LazyTile) => LazyTile,
     tt: (LazyTile, LazyTile) => LazyTile,
     ii: (Int, Int) => Boolean,
     di: (Double, Int) => Boolean,
     dd: (Double, Double) => Boolean,
     id: (Int, Double) => Boolean,
     res1: Result,
     res2: Result
   ): Result = (res1, res2) match {
     case (TileResult(lt1), TileResult(lt2)) => TileResult(tt(lt1, lt2))
     case (TileResult(lt), IntResult(int)) => TileResult((ti(lt, int)))
     case (IntResult(int), TileResult(lt)) => TileResult((it(int, lt)))
     case (TileResult(lt), DoubleResult(double)) => TileResult(td(lt, double))
     case (DoubleResult(double), TileResult(lt)) => TileResult(dt(double, lt))
     case (IntResult(int1), IntResult(int2)) => BoolResult(ii(int1, int2))
     case (DoubleResult(dbl), IntResult(int)) => BoolResult(di(dbl, int))
     case (DoubleResult(dbl1), DoubleResult(dbl2)) => BoolResult(dd(dbl1, dbl2))
     case (IntResult(int), DoubleResult(dbl)) => BoolResult(id(int, dbl))
   }

   private def tileOrScalarReduction(
     ti: (LazyTile, Int) => LazyTile,
     it: (Int, LazyTile) => LazyTile,
     td: (LazyTile, Double) => LazyTile,
     dt: (Double, LazyTile) => LazyTile,
     tt: (LazyTile, LazyTile) => LazyTile,
     ii: (Int, Int) => Int,
     di: (Double, Int) => Double,
     dd: (Double, Double) => Double,
     id: (Int, Double) => Double,
     res1: Result,
     res2: Result
   ): Result = (res1, res2) match {
     case (TileResult(lt1), TileResult(lt2)) => TileResult(tt(lt1, lt2))
     case (TileResult(lt), IntResult(int)) => TileResult((ti(lt, int)))
     case (IntResult(int), TileResult(lt)) => TileResult((it(int, lt)))
     case (TileResult(lt), DoubleResult(double)) => TileResult(td(lt, double))
     case (DoubleResult(double), TileResult(lt)) => TileResult(dt(double, lt))
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

  val additionTile = Directive { case (a@Addition(_), childResults) if (a.kind == MamlKind.Tile) =>
    val grouped = childResults.groupBy(_.kind)

    val scalarSums =
      (doubleResults(grouped) |@| intResults(grouped)).map { case (dbls, ints) => dbls.sum + ints.sum }

    (lazytileResults(grouped) |@| scalarSums).map { case (tiles, sums) =>
      val tileSum = tiles.reduce({ (lt1: LazyTile, lt2: LazyTile) => LazyTile.DualCombine(List(lt1, lt2), {_ + _}, {_ + _}) })
      TileResult(LazyTile.DualMap(List(tileSum), { i: Int => i + sums.toInt }, { i: Double => i + sums }))
    }
  }

  val subtraction = Directive { case (a@Subtraction(_), childResults) =>
    val results = childResults.reduce({ (res1, res2) =>
      tileReduction({_ - _}, {_ -: _}, {_ - _}, {_ -: _}, {_ - _}, res1, res2)
    })
    Valid(results)
  }

  val division = Directive { case (a@Division(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      tileReduction({_ / _}, {_ /: _}, {_ / _}, {_ /: _}, {_ / _}, res1, res2)
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

  val multiplicationTile = Directive { case (a@Multiplication(_), childResults) if (a.kind == MamlKind.Tile) =>
    val grouped = childResults.groupBy(_.kind)

    val scalarProduct =
      (doubleResults(grouped) |@| intResults(grouped)).map { case (dbls, ints) => dbls.product * ints.product }

    (lazytileResults(grouped) |@| scalarProduct).map { case (tiles, product) =>
      val tileProduct = tiles.reduce({ (lt1: LazyTile, lt2: LazyTile) => LazyTile.DualCombine(List(lt1, lt2), {_ * _}, {_ * _}) })
      TileResult(LazyTile.DualMap(List(tileProduct), { i: Int => i * product.toInt }, { i: Double => i * product }))
    }
  }

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

  val maxTile = Directive { case (a@Max(_), childResults) if (a.kind == MamlKind.Tile) =>
    val grouped = childResults.groupBy(_.kind)

    val scalarMax: Interpreted[Option[Double]] =
      (doubleResults(grouped) |@| intResults(grouped)).map { case (dbls, ints) =>
        (Try(dbls.max).toOption, Try(ints.max).toOption) match {
          case (Some(dbl), Some(int)) => Some(dbl max int)
          case (None, Some(int)) => Some(int)
          case (Some(dbl), None) => Some(dbl)
          case _ => None
        }
      }

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

  val minTile = Directive { case (a@Min(_), childResults) if (a.kind == MamlKind.Tile) =>
    val grouped = childResults.groupBy(_.kind)

    val scalarMin: Interpreted[Option[Double]] =
      (doubleResults(grouped) |@| intResults(grouped)).map { case (dbls, ints) =>
        (Try(dbls.min).toOption, Try(ints.min).toOption) match {
          case (Some(dbl), Some(int)) => Some(dbl min int)
          case (None, Some(int)) => Some(int)
          case (Some(dbl), None) => Some(dbl)
          case _ => None
        }
      }

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
        {_ < _}, { (i, t) => t < i }, {_ < _}, { (d, t) => t < d }, {_ < _},
        {_ < _}, {_ < _}, {_ < _}, {_ < _.toInt},
        res1, res2
      )
    })
    Valid(results)
  }

  val lessThanOrEqualTo = Directive { case (a@LessOrEqual(_), childResults) =>
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

  val notEqualTo = Directive { case (a@Equal(_), childResults) =>
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
}
