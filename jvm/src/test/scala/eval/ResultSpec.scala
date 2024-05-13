package com.azavea.maml.eval

import com.azavea.maml.error._
import com.azavea.maml.ast._
import com.azavea.maml.eval._
import com.azavea.maml.eval.tile._
import com.azavea.maml.eval.directive.SourceDirectives._
import com.azavea.maml.eval.directive.OpDirectives._

import geotrellis.raster._
import geotrellis.vector._
import geotrellis.proj4.WebMercator
import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ResultSpec extends AnyFunSpec with Matchers {

  it("Evaluate to desired output (int)") {
    IntResult(42 + 1).as[Int] should be(Valid(43))
  }

  it("Evaluate to desired output (double)") {
    IntResult(42 + 1).as[Double] should be(Valid(43.0))
  }

  it("Evaluate to desired output (tile)") {
    val anImage = ImageResult(LazyMultibandRaster(List(LazyRaster(IntArrayTile(1 to 4 toArray, 2, 2), Extent(0, 0, 1, 1), WebMercator))))
    val anInt = IntResult(1)

    anImage.as[Int] should be(Invalid(NEL.of(DivergingTypes("int", List("img")))))
    anImage.as[MultibandTile] should matchPattern { case Valid(_) => }
    anInt.as[MultibandTile] should matchPattern { case Invalid(_) => }

    val complexImage = ImageResult(
      LazyMultibandRaster(
        List(
          LazyRaster.MapInt(List(LazyRaster(IntArrayTile(1 to 4 toArray, 2, 2), Extent(0, 0, 1, 1), WebMercator)), { i: Int => i + 4 })
        )
      )
    )
    complexImage.as[MultibandTile] should matchPattern { case Valid(_) => }
  }

  it("Evaluate float tile with different cols / rows") {
    val zero = LazyRaster(FloatArrayTile.fill(0, 52, 36), Extent(0, 0, 4, 4), WebMercator)
    val one = LazyRaster(FloatArrayTile.fill(1, 52, 36), Extent(0, 0, 4, 4), WebMercator)
    val tr = ImageResult(LazyMultibandRaster(List(LazyRaster.DualCombine(List(zero, one), _ - _, _ - _))))
    val tile = tr.as[MultibandTile].valueOr(r => throw new Exception(r.toString))

    tr.res.bands.head._2.cols should be(zero.cols)
    tr.res.bands.head._2.rows should be(zero.rows)

    tr.res.bands.head._2.cols should be(tile.cols)
    tr.res.bands.head._2.rows should be(tile.rows)
  }

  it("Evaluate mask out data according to a mask") {
    val rasterOnes = LazyRaster(IntArrayTile(1 to 16 toArray, 4, 4), Extent(0, 0, 3, 3), WebMercator)
    val mask = MultiPolygon(Extent(0, 0, 1, 1).toPolygon)
    val maskResult = ImageResult(LazyMultibandRaster(List(MaskingNode(List(rasterOnes), mask))))

    val maskResultSB = ImageResult(LazyMultibandRaster(List(MaskingNode(List(rasterOnes), mask))))
    val maskResultRGB = ImageResult(LazyMultibandRaster(List(rasterOnes, rasterOnes, rasterOnes)).mask(mask))

    for {
      x <- (0 to 3).toArray
      y <- (0 to 3).toArray
    } yield {
      val fetchedSB = maskResultSB.res.bands.head._2.get(x, y)
      val fetchedRGB = maskResultRGB.res.bands.toList.map { _._2.get(x, y) }
      if ((x, y) == (0, 3)) {
        isData(fetchedSB) should be(true)
        fetchedRGB.map { isData(_) } should be(List(true, true, true))
      } else {
        isData(fetchedSB) should be(false)
        fetchedRGB.map { isData(_) } should be(List(false, false, false))
      }
    }
  }
}
