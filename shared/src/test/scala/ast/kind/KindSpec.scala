package com.azavea.maml.ast.kind

import com.azavea.maml.ast._
import com.azavea.maml.util._

import org.scalatest._
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import cats._
import cats.data._
import Validated._


class KindSpec extends FunSpec with Matchers {
  it("Typecheck a valid focal tree") {
    FocalMax(List(TileSource("test")), Square(1))
  }

  it("Should correctly determine the output type for a foldable operation (tile)") {
    Max(List(IntLiteral(42), TileSource("test"), IntLiteral(51))).kind should be (MamlKind.Tile)
  }

  it("Should correctly determine the output type for a foldable operation (scalar)") {
    Max(List(IntLiteral(42), IntLiteral(51))).kind should be (MamlKind.Scalar)
  }
}
