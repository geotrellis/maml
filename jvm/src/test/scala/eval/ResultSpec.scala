package com.azavea.maml.eval

import com.azavea.maml.ast._
import com.azavea.maml.dsl.jvm._
import com.azavea.maml.error._
import com.azavea.maml.eval.tile._
import com.azavea.maml.eval.directive.SourceDirectives._
import com.azavea.maml.eval.directive.OpDirectives._

import geotrellis.raster._
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
    TileResult(LazyTile(IntArrayTile(1 to 4 toArray, 2, 2))).as[Int] should be (Invalid(NEL.of(EvalTypeError("int", List("Tile")))))
    IntResult(1).as[Tile] should matchPattern { case Invalid(_) => }

    TileResult(LazyTile(IntArrayTile(1 to 4 toArray, 2, 2))).as[Tile] should matchPattern { case Valid(_) => }

    TileResult(LazyTile.MapInt(List(LazyTile(IntArrayTile(1 to 4 toArray, 2, 2))), { i: Int => i + 4 }))
      .as[Tile] should matchPattern { case Valid(_) => }
  }
}
