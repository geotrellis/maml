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

  it("Should correctly determine RGB output type for a foldable operation (scalar)") {
    RGB(List(RasterVar("test1"), RasterVar("test2"), RasterVar("test3"))).kind should be (MamlKind.Image)
  }

  it("Should correctly determine RGB with Rescale output type for a foldable operation (scalar)") {
    RGB(List(Rescale(RasterVar("test1") :: Nil, 10, 20), Rescale(RasterVar("test2") :: Nil, 30, 40), Rescale(RasterVar("test3") :: Nil, 50, 60))).kind should be (MamlKind.Image)
  }
}
