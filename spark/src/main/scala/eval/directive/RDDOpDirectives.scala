package com.azavea.maml.spark.eval.directive

import com.azavea.maml._
import com.azavea.maml.ast._
import com.azavea.maml.eval._
import com.azavea.maml.spark.eval._
import com.azavea.maml.eval.directive._
import com.azavea.maml.util.{NeighborhoodConversion => NC}

import cats._
import cats.data.{NonEmptyList => NEL, _}
import cats.data.Validated._
import cats.implicits._
import geotrellis.raster._
import geotrellis.raster.render._
import geotrellis.raster.mapalgebra.{local => gt}
import geotrellis.spark._
import geotrellis.spark.render._
import geotrellis.vector._
import org.apache.spark._
import rdd._


object RDDOpDirectives {
  private def doubleResults(grouped: Map[MamlKind, Seq[Result]]): Interpreted[List[Double]] =
    grouped.getOrElse(MamlKind.Double, List.empty).map(_.as[Double]).toList.sequence

  private def intResults(grouped: Map[MamlKind, Seq[Result]]): Interpreted[List[Int]] =
    grouped.getOrElse(MamlKind.Int, List.empty).map(_.as[Int]).toList.sequence

  private def spatialRDDResults(grouped: Map[MamlKind, Seq[Result]]): Interpreted[List[TileLayerRDD[SpatialKey]]] =
    grouped(MamlKind.Tile).map(_.as[TileLayerRDD[SpatialKey]]).toList.sequence

  /** Some sugar to wrap a common pattern. */
  def unary(f: RDD[(SpatialKey,Tile)] => RDD[(SpatialKey,Tile)], r: TileLayerRDD[SpatialKey]): Interpreted[Result] =
    Valid(RDDResult(r.withContext(f(_))))

  /** Perform a binary operation on RDDs, while preserving any metadata they had. */
  private def binary(
    fn: (RDD[(SpatialKey, Tile)], RDD[(SpatialKey, Tile)]) => RDD[(SpatialKey, Tile)],
    rdd1: TileLayerRDD[SpatialKey],
    rdd2: TileLayerRDD[SpatialKey]
  ): TileLayerRDD[SpatialKey] = {
    TileLayerRDD(fn(rdd1, rdd2), rdd1.metadata.combine(rdd2.metadata))
  }

  /** No, `ri` and `ir` are not the same thing, since order is significant for
    * subtraction and division.
    */
  private def reduce(
    ri: (RDD[(SpatialKey, Tile)], Int) => RDD[(SpatialKey, Tile)],
    ir: (Int, RDD[(SpatialKey, Tile)]) => RDD[(SpatialKey, Tile)],
    rd: (RDD[(SpatialKey, Tile)], Double) => RDD[(SpatialKey, Tile)],
    dr: (Double, RDD[(SpatialKey, Tile)]) => RDD[(SpatialKey, Tile)],
    rr: (RDD[(SpatialKey, Tile)], RDD[(SpatialKey, Tile)]) => RDD[(SpatialKey, Tile)],
    res1: Result,
    res2: Result
  ): Result = (res1, res2) match {
    case (RDDResult(r1), RDDResult(r2)) => RDDResult(binary(rr, r1, r2))
    case (RDDResult(rdd), IntResult(int)) => RDDResult(rdd.withContext(ri(_, int)))
    case (IntResult(int), RDDResult(rdd)) => RDDResult(rdd.withContext(ir(int, _)))
    case (RDDResult(rdd), DoubleResult(double)) => RDDResult(rdd.withContext(rd(_, double)))
    case (DoubleResult(double), RDDResult(rdd)) => RDDResult(rdd.withContext(dr(double, _)))
  }

  /** Sugar for a common pattern with the unary math operations. */
  private def mathy(
    fr: RDD[(SpatialKey, Tile)] => RDD[(SpatialKey, Tile)],
    fi: Int => Result,
    fd: Double => Result,
    res: Result
  ): Interpreted[Result] = res match {
    case RDDResult(r) => unary(fr, r)
    case IntResult(i) => Valid(fi(i))
    case DoubleResult(d) => Valid(fd(d))
  }

  /* --- FOLDABLE EXPRESSIONS --- */

  val addition = Directive { case (a@Addition(_), childResults) =>
    val results: Result = childResults.reduce { (res1, res2) =>
      reduce({_ + _}, {_ +: _}, {_ + _}, {_ +: _}, {_ + _}, res1, res2)
    }

    Valid(results)
  }

  val subtraction = Directive { case (a@Subtraction(_), childResults) =>
    val results: Result = childResults.reduce { (res1, res2) =>
      reduce({_ - _}, {_ -: _}, {_ - _}, {_ -: _}, {_ - _}, res1, res2)
    }

    Valid(results)
  }

  val multiplication = Directive { case (a@Multiplication(_), childResults) =>
    val results: Result = childResults.reduce { (res1, res2) =>
      reduce({_ * _}, {_ *: _}, {_ * _}, {_ *: _}, {_ * _}, res1, res2)
    }

    Valid(results)
  }

  val division = Directive { case (a@Division(_), childResults) =>
    val results: Result = childResults.reduce { (res1, res2) =>
      reduce({_ / _}, {_ /: _}, {_ / _}, {_ /: _}, {_ / _}, res1, res2)
    }

    Valid(results)
  }

  val max = Directive { case (Max(_), childResults) =>
    val results: Result = childResults.reduce { (res1, res2) =>
      reduce({_.localMax(_)}, {(i,r) => r.localMax(i)}, {_.localMax(_)}, {(d,r) => r.localMax(d)}, {_.localMax(_)}, res1, res2)
    }

    Valid(results)
  }

  val min = Directive { case (Min(_), childResults) =>
    val results: Result = childResults.reduce { (res1, res2) =>
      reduce({_.localMin(_)}, {(i,r) => r.localMin(i)}, {_.localMin(_)}, {(d,r) => r.localMin(d)}, {_.localMin(_)}, res1, res2)
    }

    Valid(results)
  }

  val or = Directive { case (Or(_), childResults) =>
    val results: Result = childResults.reduce { (res1, res2) =>
      reduce({_ | _}, {_ |: _}, {_ | d2i(_)}, {d2i(_) |: _}, {_ | _}, res1, res2)
    }

    Valid(results)
  }

  val and = Directive { case (And(_), childResults) =>
    val results: Result = childResults.reduce { (res1, res2) =>
      reduce({_ & _}, {_ &: _}, {_ & d2i(_)}, {d2i(_) &: _}, {_ localAnd _}, res1, res2)
    }

    Valid(results)
  }

  val xor = Directive { case (Xor(_), childResults) =>
    val rr: (RDD[(SpatialKey, Tile)], RDD[(SpatialKey, Tile)]) => RDD[(SpatialKey, Tile)] = { (r1, r2) =>
      r1.combineValues(r2) { (t1, t2) => t1.combineDouble(t2)(gt.Xor.combine) }
    }

    val results: Result = childResults.reduce { (res1, res2) =>
      reduce({_ ^ _}, {_ ^: _}, {_ ^ d2i(_)}, {d2i(_) ^: _}, rr, res1, res2)
    }

    Valid(results)
  }

  /* --- BINARY EXPRESSIONS --- */

  val equalTo = Directive { case (Equal(_), res1 :: res2 :: Nil) =>
    val results: Result =
      reduce(
        {_.localEqual(_)}, {(i,r) => r.localEqual(i)},
        {_.localEqual(_)}, {(d,r) => r.localEqual(d)},
        {_.localEqual(_)}, res1, res2)

    Valid(results)
  }

  val unequalTo = Directive { case (Unequal(_), res1 :: res2 :: Nil) =>
    val results: Result =
      reduce(
        {_.localUnequal(_)}, {(i,r) => r.localUnequal(i)},
        {_.localUnequal(_)}, {(d,r) => r.localUnequal(d)},
        {_.localUnequal(_)}, res1, res2)

    Valid(results)
  }

  val lessThan = Directive { case (Less(_), res1 :: res2 :: Nil) =>
    Valid(reduce({_ < _}, {_ <<: _}, {_ < _}, {_ <<: _}, {_ < _}, res1, res2))
  }

  val lessThanOrEqualTo = Directive { case (LessOrEqual(_), res1 :: res2 :: Nil) =>
    Valid(reduce({_ <= _}, {_ <=: _}, {_ <= _}, {_ <=: _}, {_ <= _}, res1, res2))
  }

  val greaterThan = Directive { case (Greater(_), res1 :: res2 :: Nil) =>
    Valid(reduce({_ > _}, {_ >>: _}, {_ > _}, {_ >>: _}, {_ > _}, res1, res2))
  }

  val greaterThanOrEqualTo = Directive { case (GreaterOrEqual(_), res1 :: res2 :: Nil) =>
    Valid(reduce({_ >= _}, {_ >=: _}, {_ >= _}, {_ >=: _}, {_ >= _}, res1, res2))
  }

  val masking = Directive {
    case (Masking(_), RDDResult(r) :: GeomResult(g: Polygon) :: Nil) => Valid(RDDResult(r.mask(g)))
    case (Masking(_), GeomResult(g: Polygon) :: RDDResult(r) :: Nil) => Valid(RDDResult(r.mask(g)))
    case (Masking(_), RDDResult(r) :: GeomResult(g: MultiPolygon) :: Nil) => Valid(RDDResult(r.mask(g)))
    case (Masking(_), GeomResult(g: MultiPolygon) :: RDDResult(r) :: Nil) => Valid(RDDResult(r.mask(g)))
  }

  val atan2 = Directive { case (Atan2(_), childResults) =>
    val ri: (TileLayerRDD[SpatialKey], Int) => TileLayerRDD[SpatialKey] = { (r, i) =>
      r.withContext(_.mapValues(_.mapDouble(math.atan2(i.toDouble, _))))
    }

    val rd: (TileLayerRDD[SpatialKey], Double) => TileLayerRDD[SpatialKey] = { (r, d) =>
      r.withContext(_.mapValues(_.mapDouble(math.atan2(d, _))))
    }

    val results: Result = childResults match {
      case (RDDResult(r1) :: RDDResult(r2) :: Nil) =>
        RDDResult(binary({ (a,b) => a.combineValues(b) { (t1,t2) => t1.combineDouble(t2)(math.atan2) }}, r1, r2))
      case (RDDResult(r) :: IntResult(i) :: Nil) => RDDResult(ri(r, i))
      case (IntResult(i) :: RDDResult(r) :: Nil) => RDDResult(ri(r, i))
      case (RDDResult(r) :: DoubleResult(d) :: Nil) => RDDResult(rd(r, d))
      case (DoubleResult(d) :: RDDResult(r) :: Nil) => RDDResult(rd(r, d))
    }

    Valid(results)
  }

  val pow = Directive { case (Pow(_), res1 :: res2 :: Nil) =>
    val f: (RDD[(SpatialKey, Tile)], RDD[(SpatialKey, Tile)]) => RDD[(SpatialKey, Tile)] = { (a,b) =>
      a.combineValues(b) { (t1,t2) => t1.combineDouble(t2)(math.pow) }
    }

    Valid(reduce({_ localPow _}, { (i,r) => r.localPow(i) }, { _ localPow _ }, { (d,r) => r.localPow(d) }, f, res1, res2))
  }

  /* --- UNARY EXPRESSIONS --- */

  val classify = Directive { case (Classification(_, classMap), RDDResult(r) :: Nil) =>
    unary({ _.color(ColorMap(classMap.classifications)) }, r)
  }

  val sin = Directive { case (Sin(_), res :: Nil) =>
    mathy({_.localMapDouble(math.sin(_))}, { i => DoubleResult(math.sin(i.toDouble)) }, { d => DoubleResult(math.sin(d)) }, res)
  }
  val cos = Directive { case (Cos(_), res :: Nil) =>
    mathy({_.localMapDouble(math.cos(_))}, { i => DoubleResult(math.cos(i.toDouble)) }, { d => DoubleResult(math.cos(d)) }, res)
  }
  val tan = Directive { case (Tan(_), res :: Nil) =>
    mathy({_.localMapDouble(math.tan(_))}, { i => DoubleResult(math.tan(i.toDouble)) }, { d => DoubleResult(math.tan(d)) }, res)
  }

  val sinh = Directive { case (Sinh(_), res :: Nil) =>
    mathy({_.localMapDouble(math.sinh(_))}, { i => DoubleResult(math.sinh(i.toDouble)) }, { d => DoubleResult(math.sinh(d)) }, res)
  }
  val cosh = Directive { case (Cosh(_), res :: Nil) =>
    mathy({_.localMapDouble(math.cosh(_))}, { i => DoubleResult(math.cosh(i.toDouble)) }, { d => DoubleResult(math.cosh(d)) }, res)
  }
  val tanh = Directive { case (Tanh(_), res :: Nil) =>
    mathy({_.localMapDouble(math.tanh(_))}, { i => DoubleResult(math.tanh(i.toDouble)) }, { d => DoubleResult(math.tanh(d)) }, res)
  }

  val asin = Directive { case (Asin(_), res :: Nil) =>
    mathy({_.localMapDouble(math.asin(_))}, { i => DoubleResult(math.asin(i.toDouble)) }, { d => DoubleResult(math.asin(d)) }, res)
  }
  val acos = Directive { case (Acos(_), res :: Nil) =>
    mathy({_.localMapDouble(math.acos(_))}, { i => DoubleResult(math.acos(i.toDouble)) }, { d => DoubleResult(math.acos(d)) }, res)
  }
  val atan = Directive { case (Atan(_), res :: Nil) =>
    mathy({_.localMapDouble(math.atan(_))}, { i => DoubleResult(math.atan(i.toDouble)) }, { d => DoubleResult(math.atan(d)) }, res)
  }

  val floor = Directive { case (Floor(_), res :: Nil) =>
    mathy({ _.localFloor }, { IntResult(_) }, { d => IntResult(math.floor(d).toInt) }, res)
  }
  val ceil = Directive { case (Ceil(_), res :: Nil) =>
    mathy({ _.localCeil }, { IntResult(_) }, { d => IntResult(math.ceil(d).toInt) }, res)
  }

  val loge  = Directive { case (LogE(_),  res :: Nil) =>
    mathy({ _.localLog }, { i => DoubleResult(math.log(i)) }, { d => DoubleResult(math.log(d)) }, res)
  }
  val log10 = Directive { case (Log10(_), res :: Nil) =>
    mathy({ _.localLog10 }, { i => DoubleResult(math.log10(i)) }, { d => DoubleResult(math.log10(d)) }, res)
  }

  val root = Directive { case (SquareRoot(_), res :: Nil) =>
    mathy({ _.localSqrt }, { i => DoubleResult(math.sqrt(i)) }, { d => DoubleResult(math.sqrt(d)) }, res)
  }
  val round = Directive { case (Round(_), res :: Nil) =>
    mathy({ _.localRound }, { i => IntResult(math.round(i)) }, { d => IntResult(math.round(d).toInt) }, res)
  }
  val abs = Directive { case (Abs(_), res :: Nil) =>
    mathy({ _.localAbs }, { i => IntResult(math.abs(i)) }, { d => DoubleResult(math.abs(d)) }, res)
  }

  val defined = Directive { case (Defined(_), res :: Nil) =>
    mathy({ _.localDefined }, { i => BoolResult(isData(i)) }, { d => BoolResult(isData(d)) }, res)
  }
  val undefined = Directive { case (Undefined(_), res :: Nil) =>
    mathy({ _.localUndefined }, { i => BoolResult(!isData(i)) }, { d => BoolResult(!isData(d)) }, res)
  }

  val numNegation = Directive { case (NumericNegation(_), res :: Nil) =>
    mathy({ _.localNegate }, { i => IntResult(i * -1) }, { d => DoubleResult(d * -1) }, res)
  }
  val logNegation = Directive { case (LogicalNegation(_), res :: Nil) =>
    mathy({ _.localNot }, { i => IntResult(if (i == 0) 1 else 0) }, { d => IntResult(if (d == 0) 1 else 0) }, res)
  }

  /* --- FOCAL EXPRESSIONS --- */
  val focalMax = Directive { case (FocalMax(_, n), RDDResult(r) :: Nil) => Valid(RDDResult(r.focalMax(NC(n)))) }
  val focalMin = Directive { case (FocalMin(_, n), RDDResult(r) :: Nil) => Valid(RDDResult(r.focalMin(NC(n)))) }
  val focalMean = Directive { case (FocalMean(_, n), RDDResult(r) :: Nil) => Valid(RDDResult(r.focalMean(NC(n)))) }
  val focalMedian = Directive { case (FocalMedian(_, n), RDDResult(r) :: Nil) => Valid(RDDResult(r.focalMedian(NC(n)))) }
  val focalMode = Directive { case (FocalMode(_, n), RDDResult(r) :: Nil) => Valid(RDDResult(r.focalMode(NC(n)))) }
  val focalSum = Directive { case (FocalSum(_, n), RDDResult(r) :: Nil) => Valid(RDDResult(r.focalSum(NC(n)))) }
  val focalStdDev = Directive { case (FocalStdDev(_, n), RDDResult(r) :: Nil) => Valid(RDDResult(r.focalStandardDeviation(NC(n)))) }
}
