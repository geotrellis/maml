package maml.eval

import maml.ast._
import maml.ast.jvm.TileLiteral
import maml.ast.utility.Square
import maml.dsl.jvm._
import maml.error._
import maml.eval.tile._
import maml.eval.directive.SourceDirectives
import maml.eval.directive.OpDirectives
import maml.eval.directive.FocalDirectives
import SourceDirectives._

import geotrellis.raster._
import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import org.scalatest._


class ScopedEvaluationSpec extends FunSpec with Matchers {

  val interpreter = Interpreter.buffering(
    ScopedDirective.pure[IntLiteral](SourceDirectives.intLiteralDirective),
    ScopedDirective.pure[BoolLiteral](SourceDirectives.boolLiteralDirective),
    ScopedDirective.pure[DoubleLiteral](SourceDirectives.dblLiteralDirective),
    ScopedDirective.pure[TileLiteral](SourceDirectives.tileLiteralDirective),
    ScopedDirective.pure[Addition](OpDirectives.additionDirectiveInt orElse OpDirectives.additionDirectiveDouble orElse OpDirectives.additionDirectiveTile),
    ScopedDirective.pure[FocalMax](FocalDirectives.focalMaxDirective)
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
