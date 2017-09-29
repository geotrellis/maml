package com.azavea.maml.eval

import com.azavea.maml.ast._
import com.azavea.maml.ast.TileLiteral
import com.azavea.maml.util.Square
import com.azavea.maml.dsl._
import com.azavea.maml.eval._
import com.azavea.maml.eval.tile._
import com.azavea.maml.eval.directive._

import geotrellis.raster._
import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import org.scalatest._

import scala.reflect._

class ScopedEvaluationSpec extends FunSpec with Matchers {

  implicit class TypeRefinement(self: Interpreted[Result]) {
    def as[T](implicit ct: ClassTag[T]): Interpreted[T] = self match {
      case Valid(r) => r.as[T]
      case i@Invalid(_) => i
    }
  }

  val interpreter = Interpreter.buffering(
    ScopedDirective.pure[IntLiteral](SourceDirectives.intLiteral),
    ScopedDirective.pure[BoolLiteral](SourceDirectives.boolLiteral),
    ScopedDirective.pure[DoubleLiteral](SourceDirectives.dblLiteral),
    ScopedDirective.pure[TileLiteral](SourceDirectives.tileLiteral),
    ScopedDirective.pure[Addition](OpDirectives.additionInt orElse OpDirectives.additionDouble orElse OpDirectives.additionTile),
    ScopedDirective.pure[Subtraction](OpDirectives.subtraction),
    ScopedDirective.pure[Multiplication](OpDirectives.multiplicationInt orElse OpDirectives.multiplicationDouble orElse OpDirectives.multiplicationTile),
    ScopedDirective.pure[Max](OpDirectives.maxInt orElse OpDirectives.maxDouble orElse OpDirectives.maxTile),
    ScopedDirective.pure[Min](OpDirectives.minInt orElse OpDirectives.minDouble orElse OpDirectives.minTile),
    ScopedDirective.pure[Division](OpDirectives.division),
    ScopedDirective.pure[Less](OpDirectives.lessThan),
    ScopedDirective.pure[LessOrEqual](OpDirectives.lessThanOrEqualTo),
    ScopedDirective.pure[Equal](OpDirectives.equalTo),
    ScopedDirective.pure[GreaterOrEqual](OpDirectives.greaterThanOrEqualTo),
    ScopedDirective.pure[Greater](OpDirectives.greaterThan),
    ScopedDirective.pure[FocalMax](FocalDirectives.focalMax),
    ScopedDirective.pure[FocalMin](FocalDirectives.focalMin),
    ScopedDirective.pure[FocalMean](FocalDirectives.focalMean),
    ScopedDirective.pure[FocalMedian](FocalDirectives.focalMedian),
    ScopedDirective.pure[FocalMode](FocalDirectives.focalMode)
  )

  it("Should interpret and evaluate focal operation") {
    interpreter(FocalMax(List(TileLiteral(IntArrayTile(1 to 4 toArray, 2, 2))), Square(1))).as[Tile] match {
      case Valid(tile) => tile.get(0, 0) should be (4)
      case i@Invalid(_) => fail(s"$i")
    }
  }

  it("Should interpret and evaluate Int literals") {
    interpreter(IntLiteral(42)).as[Int] should be (Valid(42))
    interpreter(IntLiteral(4200)).as[Int] should be (Valid(4200))
  }
}
