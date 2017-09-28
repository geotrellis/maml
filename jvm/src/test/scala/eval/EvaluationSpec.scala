package com.azavea.maml.eval

import com.azavea.maml.ast._
import com.azavea.maml.dsl._
import com.azavea.maml.eval._
import com.azavea.maml.eval.tile._
import com.azavea.maml.eval.directive.SourceDirectives._
import com.azavea.maml.eval.directive.OpDirectives._
import com.azavea.maml.ast.codec.tree.ExpressionTreeCodec

import io.circe._
import io.circe.syntax._
import geotrellis.raster._
import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import org.scalatest._

import scala.reflect._


class EvaluationSpec extends FunSpec with Matchers with ExpressionTreeCodec {

  implicit def tileIsTileLiteral(tile: Tile): TileLiteral = TileLiteral(tile)

  implicit class TypeRefinement(self: Interpreted[Result]) {
    def as[T: ClassTag]: Interpreted[T] = self match {
      case Valid(r) => r.as[T]
      case i@Invalid(_) => i
    }
  }

  val interpreter = Interpreter.naive(
    intLiteralDirective,
    dblLiteralDirective,
    boolLiteralDirective,
    tileLiteralDirective,
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

  it("Should interpret and evaluate to Boolean literals") {
    interpreter(BoolLiteral(true)).as[Boolean] should be (Valid(true))
    interpreter(false).as[Boolean] should be (Valid(false))
    interpreter(true).as[Boolean] should be (Valid(true))
  }

  it("Should interpret and evaluate to Int literals") {
    interpreter(IntLiteral(42)).as[Int] should be (Valid(42))
    interpreter(IntLiteral(4200)).as[Int] should be (Valid(4200))
  }


  it("Should interpret and evaluate to double literals") {
    interpreter(DoubleLiteral(42.0)).as[Double] should be (Valid(42.0))
    interpreter(DoubleLiteral(4200.0123)).as[Double] should be (Valid(4200.0123))
  }

  it("Should interpret and evaluate addition with scalars") {
    interpreter(IntLiteral(42) + DoubleLiteral(42)).as[Double] should be (Valid(84.0))
  }

  it("Should interpret and evaluate multiplication with scalars") {
    interpreter(IntLiteral(2) * DoubleLiteral(42)).as[Double] should be (Valid(84.0))
  }

  it("Should interpret and evaluate division with scalars") {
    interpreter(DoubleLiteral(20) / DoubleLiteral(2) / DoubleLiteral(2)).as[Double] should be (Valid(5.0))
  }

  it("Should interpret and evaluate comparisions with scalars") {
    println((DoubleLiteral(20) < DoubleLiteral(20)).asJson.noSpaces)
    interpreter(DoubleLiteral(20) < DoubleLiteral(20)).as[Boolean] should be (Valid(false))
    interpreter(DoubleLiteral(19) < DoubleLiteral(20)).as[Boolean] should be (Valid(true))
    interpreter(DoubleLiteral(29) < DoubleLiteral(20)).as[Boolean] should be (Valid(false))

    interpreter(DoubleLiteral(20) <= DoubleLiteral(20)).as[Boolean] should be (Valid(true))
    interpreter(DoubleLiteral(19) <= DoubleLiteral(20)).as[Boolean] should be (Valid(true))
    interpreter(DoubleLiteral(29) <= DoubleLiteral(20)).as[Boolean] should be (Valid(false))

    interpreter(DoubleLiteral(20) === DoubleLiteral(20)).as[Boolean] should be (Valid(true))
    interpreter(DoubleLiteral(19) === DoubleLiteral(20)).as[Boolean] should be (Valid(false))
    interpreter(DoubleLiteral(29) === DoubleLiteral(20)).as[Boolean] should be (Valid(false))

    interpreter(DoubleLiteral(20) >= DoubleLiteral(20)).as[Boolean] should be (Valid(true))
    interpreter(DoubleLiteral(19) >= DoubleLiteral(20)).as[Boolean] should be (Valid(false))
    interpreter(DoubleLiteral(29) >= DoubleLiteral(20)).as[Boolean] should be (Valid(true))

    interpreter(DoubleLiteral(20) > DoubleLiteral(20)).as[Boolean] should be (Valid(false))
    interpreter(DoubleLiteral(19) > DoubleLiteral(20)).as[Boolean] should be (Valid(false))
    interpreter(DoubleLiteral(29) > DoubleLiteral(20)).as[Boolean] should be (Valid(true))
  }

  it("Should interpret and evaluate ndvi") {
    interpreter((DoubleLiteral(5) - DoubleLiteral(2)) / (DoubleLiteral(5) + DoubleLiteral(2))).as[Double] match {
      case Valid(x) => x should be (0.42857 +- 0.001)
      case i@Invalid(_) => fail(s"$i")
    }
  }

  it("Should interpret and evaluate tile addition") {
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) + IntArrayTile(1 to 4 toArray, 2, 2)).as[Tile] match {
      case Valid(t) => t.get(0, 0) should be (2)
      case i@Invalid(_) => fail(s"$i")
    }
  }

  it("Should interpret and evaluate tile subtraction") {
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) - IntArrayTile(1 to 4 toArray, 2, 2)).as[Tile] match {
      case Valid(t) => t.get(0, 0) should be (0)
      case i@Invalid(_) => fail(s"$i")
    }
  }

  it("Should interpret and evaluate tile multiplication") {
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) * IntArrayTile(1 to 4 toArray, 2, 2)).as[Tile] match {
      case Valid(t) => t.get(1, 0) should be (4)
      case i@Invalid(_) => fail(s"$i")
    }
  }

  it("Should interpret and evaluate tile division") {
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) / IntArrayTile(1 to 4 toArray, 2, 2)).as[Tile] match {
      case Valid(t) => t.get(1, 0) should be (1)
      case i@Invalid(_) => fail(s"$i")
    }
  }

  it("should interpret and evaluate tile comparison") {
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) < IntArrayTile(2 to 5 toArray, 2, 2)).as[Tile] match {
      case Valid(t) => t.get(0, 0) should be (1)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) < IntArrayTile(1 to 4 toArray, 2, 2)).as[Tile] match {
      case Valid(t) => t.get(0, 0) should be (0)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) < IntArrayTile(0 to 3 toArray, 2, 2)).as[Tile] match {
      case Valid(t) => t.get(0, 0) should be (0)
      case i@Invalid(_) => fail(s"$i")
    }

    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) <= IntArrayTile(2 to 5 toArray, 2, 2)).as[Tile] match {
      case Valid(t) => t.get(0, 0) should be (1)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) <= IntArrayTile(1 to 4 toArray, 2, 2)).as[Tile] match {
      case Valid(t) => t.get(0, 0) should be (1)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) <= IntArrayTile(0 to 3 toArray, 2, 2)).as[Tile] match {
      case Valid(t) => t.get(0, 0) should be (0)
      case i@Invalid(_) => fail(s"$i")
    }

    interpreter(Equal(List(IntArrayTile(1 to 4 toArray, 2, 2), IntArrayTile(2 to 5 toArray, 2, 2)))).as[Tile] match {
      case Valid(t) => t.get(0, 0) should be (0)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(Equal(List(IntArrayTile(1 to 4 toArray, 2, 2), IntArrayTile(1 to 4 toArray, 2, 2)))).as[Tile] match {
      case Valid(t) => t.get(0, 0) should be (1)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(Equal(List(IntArrayTile(1 to 4 toArray, 2, 2), IntArrayTile(0 to 3 toArray, 2, 2)))).as[Tile] match {
      case Valid(t) => t.get(0, 0) should be (0)
      case i@Invalid(_) => fail(s"$i")
    }

    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) >= IntArrayTile(2 to 5 toArray, 2, 2)).as[Tile] match {
      case Valid(t) => t.get(0, 0) should be (0)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) >= IntArrayTile(1 to 4 toArray, 2, 2)).as[Tile] match {
      case Valid(t) => t.get(0, 0) should be (1)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) >= IntArrayTile(0 to 3 toArray, 2, 2)).as[Tile] match {
      case Valid(t) => t.get(0, 0) should be (1)
      case i@Invalid(_) => fail(s"$i")
    }

    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) > IntArrayTile(2 to 5 toArray, 2, 2)).as[Tile] match {
      case Valid(t) => t.get(0, 0) should be (0)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) > IntArrayTile(1 to 4 toArray, 2, 2)).as[Tile] match {
      case Valid(t) => t.get(0, 0) should be (0)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) > IntArrayTile(0 to 3 toArray, 2, 2)).as[Tile] match {
      case Valid(t) => t.get(0, 0) should be (1)
      case i@Invalid(_) => fail(s"$i")
    }
  }
}
