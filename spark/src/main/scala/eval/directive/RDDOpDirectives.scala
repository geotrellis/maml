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

  val addition = Directive { case (a@Addition(_), childResults) =>
    val grouped: Map[MamlKind, Seq[Result]] = childResults.groupBy(_.kind)

    val scalarSums: Interpreted[Double] =
      (doubleResults(grouped) |@| intResults(grouped)).map { case (dbls, ints) => dbls.sum + ints.sum }

    (spatialRDDResults(grouped) |@| scalarSums).map { case (rdds, sums) =>
      val rddSums: TileLayerRDD[SpatialKey] = rdds.reduce { (rdd1, rdd2) => binary({ _ + _ }, rdd1, rdd2) }

      RDDResult(rddSums.withContext(_.localAdd(sums)))
    }
  }

  val subtraction = Directive { case (a@Subtraction(_), childResults) =>
    val results: Result = childResults.reduce { (res1, res2) =>
      (res1, res2) match {
        case (RDDResult(r1), RDDResult(r2)) => RDDResult(binary({ _ - _ }, r1, r2))
        case (RDDResult(rdd), IntResult(int)) => RDDResult(rdd.withContext(_ - int))
        case (IntResult(int), RDDResult(rdd)) => RDDResult(rdd.withContext(_.-:(int)))
        case (RDDResult(rdd), DoubleResult(double)) => RDDResult(rdd.withContext(_ - double))
        case (DoubleResult(double), RDDResult(rdd)) => RDDResult(rdd.withContext(_.-:(double)))
      }
    }

    Valid(results)
  }

  val multiplication = Directive { case (a@Multiplication(_), childResults) =>
    val grouped: Map[MamlKind, Seq[Result]] = childResults.groupBy(_.kind)

    val scalarSums: Interpreted[Double] =
      (doubleResults(grouped) |@| intResults(grouped)).map { case (dbls, ints) =>
        dbls.fold(1.0) { (z, i) => z * i } * ints.fold(1) { (z, i) => z * i }
      }

    (spatialRDDResults(grouped) |@| scalarSums).map { case (rdds, sums) =>
      val rddSums: TileLayerRDD[SpatialKey] = rdds.reduce { (rdd1, rdd2) => binary({ _ * _ }, rdd1, rdd2) }

      RDDResult(rddSums.withContext(_.localMultiply(sums)))
    }
  }

  val division = Directive { case (a@Division(_), childResults) =>
    val results: Result = childResults.reduce { (res1, res2) =>
      (res1, res2) match {
        case (RDDResult(r1), RDDResult(r2)) => RDDResult(binary({ _ / _ }, r1, r2))
        case (RDDResult(rdd), IntResult(int)) => RDDResult(rdd.withContext(_ / int))
        case (IntResult(int), RDDResult(rdd)) => RDDResult(rdd.withContext(_./:(int)))
        case (RDDResult(rdd), DoubleResult(double)) => RDDResult(rdd.withContext(_ / double))
        case (DoubleResult(double), RDDResult(rdd)) => RDDResult(rdd.withContext(_./:(double)))
      }
    }

    Valid(results)
  }
}
