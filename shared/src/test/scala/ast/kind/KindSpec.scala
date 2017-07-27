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


class KindSpec extends FunSpec with Matchers {
  it("Typecheck a valid focal tree") {
    FocalMax(List(TileSource), Square(1))
  }

  it("Refuses to typecheck an invalid focal tree") {
    an[java.lang.IllegalArgumentException] should be thrownBy FocalMax(List(ScalarSource(42)), Square(1))
  }

  it("Should correctly determine the output type for a foldable operation (tile)") {
    Max(List(ScalarSource(42), TileSource, ScalarSource(51))).kind should be (MamlKind.Tile)
  }

  it("Should correctly determine the output type for a foldable operation (scalar)") {
    Max(List(ScalarSource(42), ScalarSource(51))).kind should be (MamlKind.Scalar)
  }
}
