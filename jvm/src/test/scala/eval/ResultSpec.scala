package com.azavea.maml.eval

import com.azavea.maml.error._
import com.azavea.maml.ast._
import com.azavea.maml.eval._
import com.azavea.maml.eval.tile._
import com.azavea.maml.eval.directive.SourceDirectives._
import com.azavea.maml.eval.directive.OpDirectives._

import geotrellis.raster._
import geotrellis.vector.Extent
import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import org.scalatest._


class ResultSpec extends FunSpec with Matchers {

  it("Evaluate to desired output (int)") {
    IntResult(42 + 1).as[Int] should be (Valid(43))
  }

  it("Evaluate to desired output (double)") {
    IntResult(42 + 1).as[Double] should be (Valid(43.0))
  }

  it("Evaluate to desired output (tile)") {
    val anImage = ImageResult(LazyMultibandRaster(List(
      LazyTile(IntArrayTile(1 to 4 toArray, 2, 2), Extent(0,0,0,0)))
    ))
    val anInt = IntResult(1)

    anImage.as[Int] should be (Invalid(NEL.of(DivergingTypes("int", List("img")))))
    anImage.as[MultibandTile] should matchPattern { case Valid(_) => }
    anInt.as[MultibandTile] should matchPattern { case Invalid(_) => }

    val complexImage = ImageResult(LazyMultibandRaster(List(
      LazyTile.MapInt(List(LazyTile(IntArrayTile(1 to 4 toArray, 2, 2), Extent(0,0,0,0))), { i: Int => i + 4 })
    )))
    complexImage.as[MultibandTile] should matchPattern { case Valid(_) => }
  }

  it("Evaluate float tile with different cols / rows") {
    val zero = LazyTile(Raster(FloatArrayTile.fill(0, 52, 36), Extent(0, 0, 4, 4)))
    val one = LazyTile(Raster(FloatArrayTile.fill(1, 52, 36), Extent(0, 0, 4, 4)))
    val tr = ImageResult(LazyMultibandRaster(List(
      LazyTile.DualCombine(List(zero, one), _ - _, _ - _))
    ))
    val tile = tr.as[MultibandTile].valueOr(r => throw new Exception(r.toString))

    tr.res.bands.head._2.cols should be (zero.cols)
    tr.res.bands.head._2.rows should be (zero.rows)

    tr.res.bands.head._2.cols should be (tile.cols)
    tr.res.bands.head._2.rows should be (tile.rows)
  }

  it("Evaluate mask out data according to a mask") {
    val rasterOnes = LazyTile(Raster(IntArrayTile(1 to 16 toArray, 4, 4), Extent(0, 0, 3, 3)))
    val mask = Extent(0, 0, 1, 1).as[Polygon].map(MultiPolygon(_)).get
    val maskResult = ImageResult(MaskingNode(List(rasterOnes), mask))

    isData(maskResult.res.bands.head.get(3, 3)) should be false
    isData(maskResult.res.bands.head.get(0, 0)) should be true
  }
}
