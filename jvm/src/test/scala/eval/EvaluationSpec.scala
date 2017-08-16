package maml.eval

import maml.ast._
import maml.error._
import maml.eval.tile._

import geotrellis.raster._
import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import org.scalatest._


class EvaluationSpec extends FunSpec with Matchers {
  it("Evaluate to desired output (tile)") {
    TileResult(LazyTile(IntArrayTile(1 to 4 toArray, 2, 2)))
      .as[Tile] should matchPattern { case Valid(_) => }
    TileResult(LazyTile(IntArrayTile(1 to 4 toArray, 2, 2)))
      .as[Int] should be (Invalid(NEL.of(EvalTypeError("int", List("tile")))))
  }

  it("Evaluate to desired output") {
    IntResult(42 + 1).as[Int] should be (Valid(43))
    IntResult(42 + 1).as[Double] should be (Valid(43.0))
    IntResult(42 + 1).as[Int] should be (Valid(43))
    IntResult(1).as[Tile] should matchPattern { case Invalid(_) => }
    TileResult(LazyTile.MapInt(Array(LazyTile(IntArrayTile(1 to 4 toArray, 2, 2))), { i: Int => i + 4 }))
      .as[Tile] should matchPattern { case Valid(_) => }
  }
}

