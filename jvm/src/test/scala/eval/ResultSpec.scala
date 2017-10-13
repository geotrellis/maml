package com.azavea.maml.eval

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
    ScalarResult(42 + 1).as[Int] should be (Valid(43))
  }

  it("Evaluate to desired output (double)") {
    ScalarResult(42 + 1).as[Double] should be (Valid(43.0))
  }

  it("Evaluate to desired output (tile)") {
    TileResult(LazyTile(IntArrayTile(1 to 4 toArray, 2, 2), Extent(0,0,0,0))).as[Int] should be (Invalid(NEL.of(EvalTypeError("int", List("Tile")))))
    ScalarResult(1).as[Tile] should matchPattern { case Invalid(_) => }

    TileResult(LazyTile(IntArrayTile(1 to 4 toArray, 2, 2), Extent(0,0,0,0))).as[Tile] should matchPattern { case Valid(_) => }

    TileResult(LazyTile.MapInt(List(LazyTile(IntArrayTile(1 to 4 toArray, 2, 2), Extent(0,0,0,0))), { i: Int => i + 4 }))
      .as[Tile] should matchPattern { case Valid(_) => }
  }
}
