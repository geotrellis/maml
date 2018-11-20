package com.azavea.maml.eval

import com.azavea.maml.ast._
import com.azavea.maml.eval.tile._
import geotrellis.raster._
import geotrellis.vector._

import org.scalatest._


class MultibandSelectionSpec extends FunSpec with Matchers {

  def someRaster(v: Int) = {
    val someTile = ArrayTile(Array(v, v, v, v), 2, 2)
    val someExtent = Extent(0, 0, 1, 1)
    LazyTile(someTile, someExtent)
  }

  it("Should allow for the selection of bands by idx") {
    val imagery = LazyMultibandRaster(List(someRaster(1), someRaster(2), someRaster(3)))
    imagery.select(List("1", "2")) should be (LazyMultibandRaster(Map("1" -> someRaster(2), "2" -> someRaster(3))))
  }

  it("Should allow for the selection of bands by label") {
    val imagery = LazyMultibandRaster(Map("red" -> someRaster(1), "green" -> someRaster(2), "blue" -> someRaster(3)))
    imagery.select(List("green", "blue")) should be (LazyMultibandRaster(Map("green" -> someRaster(2), "blue" -> someRaster(3))))
  }

  it("Should allow for selection of bands which encode transformations") {
    val ast =
      Addition(List(
        RasterLit(LazyMultibandRaster(Map("green" -> someRaster(3)))),
        ImageSelect(List(RasterLit(LazyMultibandRaster(Map("red" -> someRaster(1), "green" -> someRaster(3))))), List("green"))
      ))

    BufferingInterpreter.DEFAULT(ast).andThen(_.as[MultibandTile]).map(_.bandCount).toOption should be (Some(1))
  }
}
