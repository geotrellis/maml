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
import geotrellis.vector._
import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import org.scalatest._

import scala.reflect._


class VariableSpec extends FunSpec with Matchers with ExpressionTreeCodec {

  implicit class TypeRefinement(self: Interpreted[Result]) {
    def as[T: ClassTag]: Interpreted[T] = self match {
      case Valid(r) => r.as[T]
      case i@Invalid(_) => i
    }
  }

  val interpreter = NaiveInterpreter.DEFAULT

  it("should produce an accurate variable map in a simple case") {
    BoolVar("predicate1").varMap should be (Map("predicate1" -> MamlKind.Bool))
  }

  it("should produce an accurate variable map in a complex case") {
    Addition(List(IntVar("arg1"), IntVar("arg2"))).varMap should be (Map("arg1" -> MamlKind.Int, "arg2" -> MamlKind.Int))
  }
}
