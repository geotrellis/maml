package com.azavea.maml.eval

import com.azavea.maml.ast._
import com.azavea.maml.util.Square
import com.azavea.maml.dsl._
import com.azavea.maml.eval._
import com.azavea.maml.eval.tile._
import com.azavea.maml.eval.directive._

import geotrellis.raster._
import geotrellis.vector._
import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import org.scalatest._

import scala.reflect._

class ScopedEvaluationSpec extends FunSpec with Matchers {

  def tileToLit(tile: Tile): RasterLit[Raster[Tile]] = RasterLit(Raster(tile, Extent(0, 0, 0, 0)))

  implicit class TypeRefinement(self: Interpreted[Result]) {
    def as[T](implicit ct: ClassTag[T]): Interpreted[T] = self match {
      case Valid(r) => r.as[T]
      case i@Invalid(_) => i
    }
  }

  val interpreter = BufferingInterpreter.DEFAULT

  it("Should interpret and evaluate focal operation") {
    interpreter(FocalMax(List(tileToLit(IntArrayTile(1 to 4 toArray, 2, 2))), Square(1))).as[Tile] match {
      case Valid(tile) => tile.get(0, 0) should be (4)
      case i@Invalid(_) => fail(s"$i")
    }
  }

  it("Should interpret and evaluate Int literals") {
    interpreter(IntLit(42)).as[Int] should be (Valid(42))
    interpreter(IntLit(4200)).as[Int] should be (Valid(4200))
  }
}
