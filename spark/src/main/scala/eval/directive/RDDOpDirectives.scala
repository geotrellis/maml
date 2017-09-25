package maml.spark.eval.directive

import maml.spark._
import maml.spark.eval._
import maml.spark.ast._

import maml.ast._
import maml.eval._
import maml.eval.directive._

import geotrellis.raster._
import geotrellis.spark._

import org.apache.spark.rdd._

import cats._
import cats.implicits._
import cats.data.{NonEmptyList => NEL, _}
import Validated._


object RDDOpDirectives {
  private def doubleResults(grouped: Map[MamlKind, Seq[Result]]): Interpreted[List[Double]] =
    grouped.getOrElse(MamlKind.Double, List.empty)
      .map({ _.as[Double] })
      .toList
      .sequence

  private def intResults(grouped: Map[MamlKind, Seq[Result]]): Interpreted[List[Int]] =
    grouped.getOrElse(MamlKind.Int, List.empty)
      .map({ _.as[Int] })
      .toList
      .sequence

  private def spatialRDDResults(grouped: Map[MamlKind, Seq[Result]]): Interpreted[List[TileLayerRDD[SpatialKey]]] =
      grouped(MamlKind.Tile)
        .map({ _.as[RDD[(SpatialKey, Tile)] with Metadata[TileLayerMetadata[SpatialKey]]] })
        .toList
        .sequence

  private def rddExecutor(
    rdd1: TileLayerRDD[SpatialKey],
    rdd2: TileLayerRDD[SpatialKey],
    fn: PartialFunction[TileLayerRDD[SpatialKey], TileLayerRDD[SpatialKey]]
  ): TileLayerRDD[SpatialKey] = {
    val md1 = rdd1.metadata
    val md2 = rdd2.metadata

    if (md1.crs != md2.crs)
      throw new Exception(s"CRS for RDD1, ${md1.crs}, does not match the CRS for RDD2, ${md2.crs}")
    else if (md1.layout != md2.layout)
      throw new Exception(s"Layout for RDD1, ${md1.layout}, does not match the Layout for RDD2, ${md2.layout}")
    else
      fn(rdd2)
  }

  val additionDirective = Directive { case (a@Addition(_), childResults) =>
    val grouped = childResults.groupBy(_.kind)

    val scalarSums =
      (doubleResults(grouped) |@| intResults(grouped)).map { case (dbls, ints) =>
        dbls.sum + ints.sum
      }

      (spatialRDDResults(grouped) |@| scalarSums).map({ case (rdds, sums) =>
        val rddSums = rdds.reduce({
          (rdd1: TileLayerRDD[SpatialKey], rdd2: TileLayerRDD[SpatialKey]) =>
            rddExecutor(rdd1, rdd2, PartialFunction( { rdd => rdd1.withContext { _ localAdd rdd } }))
        })
        SpatialRDDResult(rddSums.withContext { _ localAdd sums })
      })
  }

  val subtractionDirective = Directive { case (a@Subtraction(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      (res1, res2) match {
        case (SpatialRDDResult(rdd1), SpatialRDDResult(rdd2)) =>
          SpatialRDDResult(rddExecutor(rdd1, rdd2, PartialFunction( { rdd => rdd1.withContext{ _ - rdd } })))
        case (SpatialRDDResult(rdd), IntResult(int)) => SpatialRDDResult(rdd.withContext{ _ - int })
        case (IntResult(int), SpatialRDDResult(rdd)) => SpatialRDDResult(rdd.withContext{ _.-:(int) })
        case (SpatialRDDResult(rdd), DoubleResult(double)) => SpatialRDDResult(rdd.withContext{ _ - double })
        case (DoubleResult(double), SpatialRDDResult(rdd)) => SpatialRDDResult(rdd.withContext{ _.-:(double) })
      }
    })
    Valid(results)
  }

  val multiplicationDirective = Directive { case (a@Multiplication(_), childResults) =>
    val grouped = childResults.groupBy(_.kind)

    val scalarSums =
      (doubleResults(grouped) |@| intResults(grouped)).map { case (dbls, ints) =>
        dbls.fold(1.0) { (z, i) => z * i } * ints.fold(1) { (z, i) => z * i }
      }

      (spatialRDDResults(grouped) |@| scalarSums).map({ case (rdds, sums) =>
        val rddSums = rdds.reduce({
          (rdd1: TileLayerRDD[SpatialKey], rdd2: TileLayerRDD[SpatialKey]) =>
            rddExecutor(rdd1, rdd2, PartialFunction( { rdd => rdd1.withContext { _ localMultiply rdd } }))
        })
      SpatialRDDResult(rddSums.withContext { _ localMultiply sums })
      })
  }

  val divisionDirective = Directive { case (a@Division(_), childResults) =>
    val results = childResults.reduce({ (res1: Result, res2: Result) =>
      (res1, res2) match {
        case (SpatialRDDResult(rdd1), SpatialRDDResult(rdd2)) =>
          SpatialRDDResult(rddExecutor(rdd1, rdd2, PartialFunction( { rdd => rdd1.withContext{ _ / rdd } })))
        case (SpatialRDDResult(rdd), IntResult(int)) => SpatialRDDResult(rdd.withContext{ _ / int })
        case (IntResult(int), SpatialRDDResult(rdd)) => SpatialRDDResult(rdd.withContext{ _./:(int) })
        case (SpatialRDDResult(rdd), DoubleResult(double)) => SpatialRDDResult(rdd.withContext{ _ / double })
        case (DoubleResult(double), SpatialRDDResult(rdd)) => SpatialRDDResult(rdd.withContext{ _./:(double) })
      }
    })
    Valid(results)
  }
}
