package com.azavea.maml.eval

import com.azavea.maml.util._
import com.azavea.maml.ast._
import com.azavea.maml.dsl._
import com.azavea.maml.error._
import com.azavea.maml.eval._
import com.azavea.maml.eval.tile._
import com.azavea.maml.ast.codec.tree._

import io.circe._
import io.circe.syntax._
import io.circe.generic.extras.auto._
import geotrellis.raster._
import geotrellis.vector._
import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import org.scalatest._

import scala.reflect._


class VariableSpec extends FunSpec with Matchers {

  implicit class TypeRefinement(self: Interpreted[Result]) {
    def as[T: ClassTag]: Interpreted[T] = self match {
      case Valid(r) => r.as[T]
      case i@Invalid(_) => i
    }
  }

  val interpreter = NaiveInterpreter.DEFAULT

  it("should produce an accurate variable map in a simple case") {
    Vars.vars(BoolVar("predicate1")) should be (Map("predicate1" -> MamlKind.Bool))
  }

  it("should produce an accurate variable map in a complex case") {
    Vars.vars(Addition(List(IntVar("arg1"), IntVar("arg2")))) should be (Map("arg1" -> MamlKind.Int, "arg2" -> MamlKind.Int))
  }

  it("should produce an accurate variable map with buffer in a simple case") {
    Vars.varsWithBuffer(FocalMax(List(RasterVar("someRaster")), Square(1))) should be (Map("someRaster" -> (MamlKind.Image, 1)))
  }

  it("should produce an accurate variable map with buffer in an ambiguous case") {
    val ast = Addition(List(
        FocalMax(List(
          FocalMax(List(RasterVar("someRaster")), Square(1))
        ), Square(1), TargetCell.All),
        RasterVar("someRaster")
    ))

    Vars.varsWithBuffer(ast) should be (Map("someRaster" -> (MamlKind.Image, 2)))
  }
}
