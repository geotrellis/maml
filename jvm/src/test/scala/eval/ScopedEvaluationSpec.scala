package com.azavea.maml.eval

import com.azavea.maml.error._
import com.azavea.maml.ast._
import com.azavea.maml.util.Square
import com.azavea.maml.dsl._
import com.azavea.maml.eval._
import com.azavea.maml.eval.tile._
import com.azavea.maml.eval.directive._

import geotrellis.raster._
import geotrellis.vector._
import geotrellis.proj4.WebMercator
import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.reflect._

class ScopedEvaluationSpec extends AnyFunSpec with Matchers {

  def tileToLit(tile: Tile): RasterLit[ProjectedRaster[MultibandTile]] =
    RasterLit(ProjectedRaster(MultibandTile(tile), Extent(0, 0, 1, 1), WebMercator))

  implicit class TypeRefinement(self: Interpreted[Result]) {
    def as[T](implicit ct: ClassTag[T]): Interpreted[T] = self match {
      case Valid(r)       => r.as[T]
      case i @ Invalid(_) => i
    }
  }

  val interpreter = BufferingInterpreter.DEFAULT

  it("Should interpret and evaluate focal operation") {
    interpreter(FocalMax(List(tileToLit(IntArrayTile(1 to 4 toArray, 2, 2))), Square(1))).as[MultibandTile] match {
      case Valid(tile)    => tile.bands.head.get(0, 0) should be(4)
      case i @ Invalid(_) => fail(s"$i")
    }
  }

  it("Should interpret and evaluate Int literals") {
    interpreter(IntLit(42)).as[Int] should be(Valid(42))
    interpreter(IntLit(4200)).as[Int] should be(Valid(4200))
  }
}
