package maml.eval

import maml.ast._
import maml.ast.jvm.TileLiteral
import maml.ast.utility.Square
import maml.dsl.jvm._
import maml.error._
import maml.eval._
import maml.eval.tile._
import maml.eval.directive._

import geotrellis.raster._
import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import org.scalatest._


class ScopedEvaluationSpec extends FunSpec with Matchers {

  implicit class TypeRefinement(self: Interpreted[Result]) {
    def as[T](implicit ct: ClassTag[T]): Interpreted[T] = self match {
      case Valid(r) => r.as[T](ct)
      case i@Invalid(_) => i
    }
  }

  val interpreter = Interpreter.buffering(
    ScopedDirective[BufferingInterpreter.Scope] { case (t@TileSource(id), _, scope) =>
      Valid(TileResult(LazyTile(IntArrayTile(1 to 4 toArray, 2, 2)).withBuffer(scope.buffer)))
    },
    ScopedDirective.pure[IntLiteral](SourceDirectives.intLiteralDirective),
    ScopedDirective.pure[BoolLiteral](SourceDirectives.boolLiteralDirective),
    ScopedDirective.pure[DoubleLiteral](SourceDirectives.dblLiteralDirective),
    ScopedDirective.pure[TileLiteral](SourceDirectives.tileLiteralDirective),
    ScopedDirective.pure[Addition](OpDirectives.additionDirectiveInt orElse OpDirectives.additionDirectiveDouble orElse OpDirectives.additionDirectiveTile),
    ScopedDirective.pure[Subtraction](OpDirectives.subtractionDirective),
    ScopedDirective.pure[Multiplication](OpDirectives.multiplicationDirectiveInt orElse OpDirectives.multiplicationDirectiveDouble orElse OpDirectives.multiplicationDirectiveTile),
    ScopedDirective.pure[Max](OpDirectives.maxDirectiveInt orElse OpDirectives.maxDirectiveDouble orElse OpDirectives.maxDirectiveTile),
    ScopedDirective.pure[Min](OpDirectives.minDirectiveInt orElse OpDirectives.minDirectiveDouble orElse OpDirectives.minDirectiveTile),
    ScopedDirective.pure[Division](OpDirectives.divisionDirective),
    ScopedDirective.pure[Less](OpDirectives.lessThanDirective),
    ScopedDirective.pure[LessOrEqual](OpDirectives.lessThanOrEqualToDirective),
    ScopedDirective.pure[Equal](OpDirectives.equalToDirective),
    ScopedDirective.pure[GreaterOrEqual](OpDirectives.greaterThanOrEqualToDirective),
    ScopedDirective.pure[Greater](OpDirectives.greaterThanDirective),
    ScopedDirective.pure[FocalMax](FocalDirectives.focalMaxDirective),
    ScopedDirective.pure[FocalMin](FocalDirectives.focalMinDirective),
    ScopedDirective.pure[FocalMean](FocalDirectives.focalMeanDirective),
    ScopedDirective.pure[FocalMedian](FocalDirectives.focalMedianDirective),
    ScopedDirective.pure[FocalMode](FocalDirectives.focalModeDirective)
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