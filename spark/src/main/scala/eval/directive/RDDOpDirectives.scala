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
import geotrellis.raster.Tile
import geotrellis.spark._
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
}
