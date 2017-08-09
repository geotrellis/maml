package maml.eval

import maml.ast._
import maml.eval.tile._
import maml.eval.scalar._

import geotrellis.raster._
import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import org.scalatest._


class EvaluationSpec extends FunSpec with Matchers {
  it("Evaluate to desired output (scalar)") {
    Scalar(42).as[Double] should be (Valid(42.0))
    Scalar(42).as[Int] should be (Valid(42))
    Scalar(42).as[String] should be (Invalid(NEL.of(EvalTypeError("java.lang.String", List("int", "double")))))
  }

  it("Evaluate to desired output (tile)") {
    LazyTile(IntArrayTile(1 to 4 toArray, 2, 2)).as[Tile] should matchPattern { case Valid(_) => }
    LazyTile(IntArrayTile(1 to 4 toArray, 2, 2)).as[Int] should be (Invalid(NEL.of(EvalTypeError("int", List("Tile")))))
  }

  it("Evaluate to desired output (scalar addition)") {
    Addition(List(ScalarSource(42), ScalarSource(1))).interpret.as[Double] should be (Valid(43.0))
    Addition(List(ScalarSource(42), ScalarSource(1))).interpret.as[Int] should be (Valid(43))
    Addition(List(ScalarSource(42), ScalarSource(1))).interpret.as[Tile] should matchPattern { case Invalid(_) => }
  }
}

