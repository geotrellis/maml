package com.azavea.maml.spark.eval.directive

import cats._
import cats.data.{NonEmptyList => NEL, _}
import cats.data.Validated._
import cats.implicits._
import com.azavea.maml.ast._
import com.azavea.maml.eval._
import com.azavea.maml.eval.directive._
import com.azavea.maml.spark._
import com.azavea.maml.spark.eval._
import geotrellis.raster._
import geotrellis.raster.mapalgebra.{local => gt}
import geotrellis.spark._
import geotrellis.vector._
import org.apache.spark.rdd._


object RDDOpDirectives {
  private def doubleResults(grouped: Map[MamlKind, Seq[Result]]): Interpreted[List[Double]] =
    grouped.getOrElse(MamlKind.Double, List.empty).map(_.as[Double]).toList.sequence

  private def intResults(grouped: Map[MamlKind, Seq[Result]]): Interpreted[List[Int]] =
    grouped.getOrElse(MamlKind.Int, List.empty).map(_.as[Int]).toList.sequence

  private def spatialRDDResults(grouped: Map[MamlKind, Seq[Result]]): Interpreted[List[TileLayerRDD[SpatialKey]]] =
    grouped(MamlKind.Tile).map(_.as[TileLayerRDD[SpatialKey]]).toList.sequence

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
    val results: Result = childResults.reduce { (res1, res2) =>
      reduce(
        {_ ^ _},
        {_ ^: _},
        {_ ^ d2i(_)},
        {d2i(_) ^: _},
        { (r1, r2) => r1.combineValues(r2) { (t1, t2) => t1.combineDouble(t2)(gt.Xor.combine) } },
        res1,
        res2
      )
    }

    Valid(results)
  }

  /* --- BINARY EXPRESSIONS --- */

  val equalTo = Directive { case (Equal(_), childResults) =>
    val results: Result = childResults.reduce { (res1, res2) =>
      reduce({_.localEqual(_)}, {(i,r) => r.localEqual(i)}, {_.localEqual(_)}, {(d,r) => r.localEqual(d)}, {_.localEqual(_)}, res1, res2)
    }

    Valid(results)
  }

  val unequalTo = Directive { case (Unequal(_), childResults) =>
    val results: Result = childResults.reduce { (res1, res2) =>
      reduce({_.localUnequal(_)}, {(i,r) => r.localUnequal(i)}, {_.localUnequal(_)}, {(d,r) => r.localUnequal(d)}, {_.localUnequal(_)}, res1, res2)
    }

    Valid(results)
  }

  val lessThan = Directive { case (Less(_), childResults) =>
    val results: Result = childResults.reduce { (res1, res2) =>
      reduce({_ < _}, {_ <<: _}, {_ < _}, {_ <<: _}, {_ < _}, res1, res2)
    }

    Valid(results)
  }

  val lessThanOrEqualTo = Directive { case (LessOrEqual(_), childResults) =>
    val results: Result = childResults.reduce { (res1, res2) =>
      reduce({_ <= _}, {_ <=: _}, {_ <= _}, {_ <=: _}, {_ <= _}, res1, res2)
    }

    Valid(results)
  }

  val greaterThan = Directive { case (Greater(_), childResults) =>
    val results: Result = childResults.reduce { (res1, res2) =>
      reduce({_ > _}, {_ >>: _}, {_ > _}, {_ >>: _}, {_ > _}, res1, res2)
    }

    Valid(results)
  }

  val greaterThanOrEqualTo = Directive { case (GreaterOrEqual(_), childResults) =>
    val results: Result = childResults.reduce { (res1, res2) =>
      reduce({_ >= _}, {_ >=: _}, {_ >= _}, {_ >=: _}, {_ >= _}, res1, res2)
    }

    Valid(results)
  }

  // TODO The mask can probably be something other than `Polygon`.
  val masking = Directive {
    case (Masking(_), RDDResult(r) :: GeomResult(g: Polygon) :: Nil) => Valid(RDDResult(r.mask(g)))
    case (Masking(_), GeomResult(g: Polygon) :: RDDResult(r) :: Nil) => Valid(RDDResult(r.mask(g)))
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
}
