package maml.rdd

import maml.ast._
import maml.eval._
import maml.eval.directive._
import maml.eval.directive.SourceDirectives._
import maml.dsl.jvm._

import maml.rdd.eval._
import maml.rdd.eval.directive._
import maml.rdd.eval.SpatialRDDResult._
import maml.rdd.ast._

import geotrellis.raster._
import geotrellis.spark._
import geotrellis.spark.testkit._

import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._

import org.scalatest._
import org.apache.spark._
import org.apache.spark.rdd._

import scala.reflect._


class SpatialRDDLocalOperationsSpec extends FunSpec
  with Matchers
  with TestEnvironment {

  val interpreter = Interpreter.naive(
    intLiteralDirective,
    dblLiteralDirective,
    RDDSourceDirectives.spatialRDDLiteralDirective,
    RDDOpDirectives.additionDirective,
    RDDOpDirectives.subtractionDirective,
    RDDOpDirectives.multiplicationDirective,
    RDDOpDirectives.divisionDirective
  )

  implicit class TypeRefinement(self: Interpreted[Result]) {
    def as[T: ClassTag]: Interpreted[T] = self match {
      case Valid(r) => r.as[T]
      case i@Invalid(_) => i
    }
  }

  val rdd: TileLayerRDD[SpatialKey] =
    createTileLayerRDD(IntArrayTile(0 until 10 toArray, 2, 5), TileLayout(1, 1, 2, 5))

  it("Should interpret and evaluate spatial RDDs") {
    interpreter(SpatialRDDLiteral(rdd)).as[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]] match {
      case Valid(result) => rastersEqual(rdd, result)
      case i@Invalid(_) => fail(s"$i")
    }
  }

  it("Should be able to add two spatial RDDs together") {
    val expected = {
      val tile = IntArrayTile(Array(0, 2, 4, 6, 8, 10, 12, 14, 16, 18), 2, 5)
      createTileLayerRDD(tile, TileLayout(1, 1, 2, 5))
    }

    interpreter(SpatialRDDLiteral(rdd) + SpatialRDDLiteral(rdd)).as[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]] match {
      case Valid(result) => rastersEqual(result, expected)
      case i@Invalid(_) => println(s"$i")
      }
  }

  it("Should add an int and an spatial RDD together") {
    val expected: TileLayerRDD[SpatialKey] =
      createTileLayerRDD(IntArrayTile(1 until 11 toArray, 2, 5), TileLayout(1, 1, 2, 5))

    interpreter(IntLiteral(1) + SpatialRDDLiteral(rdd)).as[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]] match {
      case Valid(result) => rastersEqual(result, expected)
      case i@Invalid(_) => println(s"$i")
      }
  }

  it("Should add a double and an spatial RDD together") {
    val expected: TileLayerRDD[SpatialKey] = {
      val tile = DoubleArrayTile(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0), 2, 5)
      createTileLayerRDD(tile, TileLayout(1, 1, 2, 5))
    }

    interpreter(DoubleLiteral(1.0) + SpatialRDDLiteral(rdd)).as[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]] match {
      case Valid(result) => rastersEqual(result, expected)
      case i@Invalid(_) => println(s"$i")
      }
  }

  it("Should subtract two spatial RDDs") {
    val expected = createTileLayerRDD(IntArrayTile.fill(0, 2, 5), TileLayout(1, 1, 2, 5))

    interpreter(SpatialRDDLiteral(rdd) - SpatialRDDLiteral(rdd)).as[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]] match {
      case Valid(result) => rastersEqual(result, expected)
      case i@Invalid(_) => println(s"$i")
      }
  }

  it("Should subtract an int from the spatial RDD") {
    val expected = {
      val tile = IntArrayTile(Array(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8), 2, 5)
      createTileLayerRDD(tile, TileLayout(1, 1, 2, 5))
    }

    interpreter(SpatialRDDLiteral(rdd) - IntLiteral(1)).as[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]] match {
      case Valid(result) => rastersEqual(result, expected)
      case i@Invalid(_) => println(s"$i")
      }
  }

  it("Should subtract an spatial RDD from a double") {
    val expected = {
      val tile = DoubleArrayTile(Array(1, 0, -1, -2, -3, -4, -5, -6, -7, -8), 2, 5)
      createTileLayerRDD(tile, TileLayout(1, 1, 2, 5))
    }

    interpreter(DoubleLiteral(1.0) - SpatialRDDLiteral(rdd)).as[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]] match {
      case Valid(result) => rastersEqual(result, expected)
      case i@Invalid(_) => println(s"$i")
      }
  }

  it("Should multiply two spatial RDDs") {
    val expected = {
      val tile = DoubleArrayTile(Array(0.0, 1.0, 4.0, 9, 16, 25, 36, 49, 64, 81), 2, 5)
      createTileLayerRDD(tile, TileLayout(1, 1, 2, 5))
    }

    interpreter(SpatialRDDLiteral(rdd) * SpatialRDDLiteral(rdd)).as[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]] match {
      case Valid(result) => rastersEqual(result, expected)
      case i@Invalid(_) => println(s"$i")
      }
  }

  it("Should multiply an spatial RDD by an int") {
    val expected = {
      val tile = IntArrayTile(Array(0, 3, 6, 9, 12, 15, 18, 21, 24, 27), 2, 5)
      createTileLayerRDD(tile, TileLayout(1, 1, 2, 5))
    }

    interpreter(SpatialRDDLiteral(rdd) * IntLiteral(3)).as[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]] match {
      case Valid(result) => rastersEqual(result, expected)
      case i@Invalid(_) => println(s"$i")
    }
  }

  it("Should multiply a double by an spatial RDD") {
    val expected = {
      val tile = IntArrayTile(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 5)
      createTileLayerRDD(tile * 5.0, TileLayout(1, 1, 2, 5))
    }

    interpreter(DoubleLiteral(5.0) * SpatialRDDLiteral(rdd)).as[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]] match {
      case Valid(result) => rastersEqual(result, expected)
      case i@Invalid(_) => println(s"$i")
      }
  }

  it("Should divide two spatial RDDs") {
    val expected = {
      val tile = DoubleArrayTile(Array(Double.NaN, 1, 1, 1, 1, 1, 1, 1, 1, 1), 2, 5)
      createTileLayerRDD(tile, TileLayout(1, 1, 2, 5))
    }

    interpreter(SpatialRDDLiteral(rdd) / SpatialRDDLiteral(rdd)).as[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]] match {
      case Valid(result) => rastersEqual(result, expected)
      case i@Invalid(_) => println(s"$i")
      }
  }

  it("Should divide an spatial RDD by an int") {
    val expected = {
      val tile = IntArrayTile(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 5)
      createTileLayerRDD(tile, TileLayout(1, 1, 2, 5))
    }

    interpreter(SpatialRDDLiteral(rdd) / IntLiteral(1)).as[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]] match {
      case Valid(result) => rastersEqual(result, expected)
      case i@Invalid(_) => println(s"$i")
      }
  }

  it("Should divide a double by an spatial RDD") {
    val expected = {
      val tile = IntArrayTile(Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), 2, 5)
      createTileLayerRDD(tile./:(5.0), TileLayout(1, 1, 2, 5))
    }

    interpreter(DoubleLiteral(5.0) / SpatialRDDLiteral(rdd)).as[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]] match {
      case Valid(result) => rastersEqual(result, expected)
      case i@Invalid(_) => println(s"$i")
      }
  }
}
