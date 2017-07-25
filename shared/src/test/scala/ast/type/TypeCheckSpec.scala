package maml.ast.kind

import maml.ast._
import maml.error._
import maml.ast.utility._

import org.scalatest._
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import cats._
import cats.data._
import Validated._


class KindCheckSpec extends FunSpec with Matchers {
  it("Typecheck an invalid focal tree") {
    val ast = FocalMax(List(TileSource), Square(1))
    KindCheck(ast) should be (Valid(MamlKind.Tile))
  }

  it("Refuses to typecheck an invalid focal tree") {
    val ast = FocalMax(List(ScalarSource(42)), Square(1))
    KindCheck(ast) should be (Invalid(NonEmptyList.of(UnaryTypeError(FocalMax(List(ScalarSource(42.0)),Square(1)), MamlKind.Scalar))))
  }

  it("Should correctly determine the output type for a foldable operation (tile)") {
    val ast = Max(List(ScalarSource(42), TileSource, ScalarSource(51)))
    KindCheck(ast) should be (Valid(MamlKind.Tile))
  }

  it("Should correctly determine the output type for a foldable operation (scalar)") {
    val ast = Max(List(ScalarSource(42), ScalarSource(51)))
    KindCheck(ast) should be (Valid(MamlKind.Scalar))
  }
}
