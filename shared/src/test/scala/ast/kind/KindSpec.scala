package com.azavea.maml.ast.kind

import com.azavea.maml.ast._
import com.azavea.maml.util._

import org.scalatest._

class KindSpec extends FunSpec with Matchers {
  it("Typecheck a valid focal tree") {
    FocalMax(List(RasterVar("test")), Square(1))
  }

  it("Should correctly determine the output type for a foldable operation (image)") {
    Max(List(IntLit(42), RasterVar("test"), IntLit(51))).kind should be (MamlKind.Image)
  }

  it("Should correctly determine the output type for a foldable operation (scalar)") {
    Max(List(IntLit(42), IntLit(51))).kind should be (MamlKind.Int)
  }
}
