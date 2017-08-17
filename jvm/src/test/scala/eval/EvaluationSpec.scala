package maml.eval

import maml.ast._
import maml.error._
import maml.eval.tile._
import maml.eval.directive.SourceDirectives._
import maml.eval.directive.OpDirectives._

import geotrellis.raster._
import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import org.scalatest._


class EvaluationSpec extends FunSpec with Matchers {

  val interpreter = Interpreter.naive(
    intLiteralDirective,
    dblLiteralDirective,
    boolLiteralDirective,
    additionDirectiveInt,
    additionDirectiveDouble,
    additionDirectiveTile,
    subtractionDirective,
    divisionDirective,
    multiplicationDirectiveInt,
    multiplicationDirectiveDouble,
    multiplicationDirectiveTile,
    maxDirectiveDouble,
    maxDirectiveInt,
    maxDirectiveTile,
    minDirectiveDouble,
    minDirectiveInt,
    minDirectiveTile,
    lessThanDirective,
    lessThanOrEqualToDirective,
    equalToDirective,
    greaterThanOrEqualToDirective,
    greaterThanDirective
  )

  it("Evaluate to desired output (tile)") {
    TileResult(LazyTile(IntArrayTile(1 to 4 toArray, 2, 2))).as[Tile] should matchPattern { case Valid(_) => }
    TileResult(LazyTile(IntArrayTile(1 to 4 toArray, 2, 2))).as[Int] should be (Invalid(NEL.of(EvalTypeError("int", List("Tile")))))
  }

  it("Evaluate to desired output") {
    IntResult(42 + 1).as[Int] should be (Valid(43))
    IntResult(42 + 1).as[Double] should be (Valid(43.0))
    IntResult(42 + 1).as[Int] should be (Valid(43))
    IntResult(1).as[Tile] should matchPattern { case Invalid(_) => }
    TileResult(LazyTile.MapInt(List(LazyTile(IntArrayTile(1 to 4 toArray, 2, 2))), { i: Int => i + 4 }))
      .as[Tile] should matchPattern { case Valid(_) => }
  }

  it("Should interpret and evaluate to Int literals") {
    interpreter(IntLiteral(42)).as[Int] should be (Valid(42))
  }

  it("Should interpret and evaluate to Boolean literals") {
    interpreter(BoolLiteral(true)).as[Boolean] should be (Valid(true))
  }

  it("Should interpret and evaluate to double literals") {
    interpreter(DoubleLiteral(42.0)).as[Double] should be (Valid(42.0))
  }

  it("Should interpret and evaluate addition with scalars") {
    interpreter(Addition(List(IntLiteral(42), DoubleLiteral(42)))).as[Double] should be (Valid(84.0))
  }

  it("Should interpret and evaluate multiplication with scalars") {
    interpreter(Multiplication(List(IntLiteral(2), DoubleLiteral(42)))).as[Double] should be (Valid(84.0))
  }

  it("Should interpret and evaluate division with scalars") {
    interpreter(Division(List(DoubleLiteral(20), DoubleLiteral(2), DoubleLiteral(2)))).as[Double] should be (Valid(5.0))
  }

  it("Should interpret and evaluate comparisions with scalars") {
    interpreter(Less(List(DoubleLiteral(20), DoubleLiteral(20)))).as[Boolean] should be (Valid(false))
    interpreter(Less(List(DoubleLiteral(19), DoubleLiteral(20)))).as[Boolean] should be (Valid(true))
    interpreter(Less(List(DoubleLiteral(29), DoubleLiteral(20)))).as[Boolean] should be (Valid(false))

    interpreter(LessOrEqual(List(DoubleLiteral(20), DoubleLiteral(20)))).as[Boolean] should be (Valid(true))
    interpreter(LessOrEqual(List(DoubleLiteral(19), DoubleLiteral(20)))).as[Boolean] should be (Valid(true))
    interpreter(LessOrEqual(List(DoubleLiteral(29), DoubleLiteral(20)))).as[Boolean] should be (Valid(false))

    interpreter(Equal(List(DoubleLiteral(20), DoubleLiteral(20)))).as[Boolean] should be (Valid(true))
    interpreter(Equal(List(DoubleLiteral(19), DoubleLiteral(20)))).as[Boolean] should be (Valid(false))
    interpreter(Equal(List(DoubleLiteral(29), DoubleLiteral(20)))).as[Boolean] should be (Valid(false))

    interpreter(GreaterOrEqual(List(DoubleLiteral(20), DoubleLiteral(20)))).as[Boolean] should be (Valid(true))
    interpreter(GreaterOrEqual(List(DoubleLiteral(19), DoubleLiteral(20)))).as[Boolean] should be (Valid(false))
    interpreter(GreaterOrEqual(List(DoubleLiteral(29), DoubleLiteral(20)))).as[Boolean] should be (Valid(true))

    interpreter(Greater(List(DoubleLiteral(20), DoubleLiteral(20)))).as[Boolean] should be (Valid(false))
    interpreter(Greater(List(DoubleLiteral(19), DoubleLiteral(20)))).as[Boolean] should be (Valid(false))
    interpreter(Greater(List(DoubleLiteral(29), DoubleLiteral(20)))).as[Boolean] should be (Valid(true))
  }

  it("Should interpret and evaluate ndvi for a single location") {
    interpreter(Division(List(
      Subtraction(List(DoubleLiteral(5), DoubleLiteral(2))),
      Addition(List(DoubleLiteral(5), DoubleLiteral(2)))
    ))).as[Double] match {
      case Valid(x) => x should be (0.42857 +- 0.001)
      case Invalid(_) => fail()
    }
  }
}

