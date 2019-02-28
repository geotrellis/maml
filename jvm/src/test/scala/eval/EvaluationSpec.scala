package com.azavea.maml.eval

import com.azavea.maml.ast._
import com.azavea.maml.dsl._
import com.azavea.maml.error._
import com.azavea.maml.eval._
import com.azavea.maml.eval.tile._
import com.azavea.maml.eval.directive.SourceDirectives._
import com.azavea.maml.eval.directive.OpDirectives._
import com.azavea.maml.ast.codec.tree.ExpressionTreeCodec
import com.azavea.maml.util.Square

import io.circe._
import io.circe.syntax._
import geotrellis.raster._
import geotrellis.vector._
import geotrellis.proj4.WebMercator
import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import org.scalatest._

import scala.reflect._


class EvaluationSpec extends FunSpec with Matchers with ExpressionTreeCodec {

  implicit def tileIsTileLiteral(tile: Tile): RasterLit[ProjectedRaster[MultibandTile]] =
    RasterLit(ProjectedRaster(MultibandTile(tile), Extent(0, 0, 0.05, 0.05), WebMercator))

  implicit class TypeRefinement(self: Interpreted[Result]) {
    def as[T: ClassTag]: Interpreted[T] = self match {
      case Valid(r) => r.as[T]
      case i@Invalid(_) => i
    }
  }

  val interpreter = Interpreter.DEFAULT

  it("Should interpret and evaluate to Boolean literals") {
    interpreter(BoolLit(true)).as[Boolean] should be (Valid(true))
    interpreter(false).as[Boolean] should be (Valid(false))
    interpreter(true).as[Boolean] should be (Valid(true))
  }

  it("Should interpret and evaluate to Int literals") {
    interpreter(IntLit(42)).as[Int] should be (Valid(42))
    interpreter(IntLit(4200)).as[Int] should be (Valid(4200))
  }


  it("Should interpret and evaluate to double literals") {
    interpreter(DblLit(42.0)).as[Double] should be (Valid(42.0))
    interpreter(DblLit(4200.0123)).as[Double] should be (Valid(4200.0123))
  }

  it("Should interpret and evaluate addition with scalars") {
    interpreter(IntLit(42) + DblLit(42)).as[Double] should be (Valid(84.0))
  }

  it("Should interpret and evaluate multiplication with scalars") {
    interpreter(IntLit(2) * DblLit(42)).as[Double] should be (Valid(84.0))
  }

  it("Should interpret and evaluate division with scalars") {
    interpreter(DblLit(20) / DblLit(2) / DblLit(2)).as[Double] should be (Valid(5.0))
  }

  it("Should interpret and evaluate comparisions with scalars") {
    interpreter(DblLit(20) < DblLit(20)).as[Boolean] should be (Valid(false))
    interpreter(DblLit(19) < DblLit(20)).as[Boolean] should be (Valid(true))
    interpreter(DblLit(29) < DblLit(20)).as[Boolean] should be (Valid(false))

    interpreter(DblLit(20) <= DblLit(20)).as[Boolean] should be (Valid(true))
    interpreter(DblLit(19) <= DblLit(20)).as[Boolean] should be (Valid(true))
    interpreter(DblLit(29) <= DblLit(20)).as[Boolean] should be (Valid(false))

    interpreter(DblLit(20) === DblLit(20)).as[Boolean] should be (Valid(true))
    interpreter(DblLit(19) === DblLit(20)).as[Boolean] should be (Valid(false))
    interpreter(DblLit(29) === DblLit(20)).as[Boolean] should be (Valid(false))

    interpreter(DblLit(20) >= DblLit(20)).as[Boolean] should be (Valid(true))
    interpreter(DblLit(19) >= DblLit(20)).as[Boolean] should be (Valid(false))
    interpreter(DblLit(29) >= DblLit(20)).as[Boolean] should be (Valid(true))

    interpreter(DblLit(20) > DblLit(20)).as[Boolean] should be (Valid(false))
    interpreter(DblLit(19) > DblLit(20)).as[Boolean] should be (Valid(false))
    interpreter(DblLit(29) > DblLit(20)).as[Boolean] should be (Valid(true))
  }

  it("Should interpret and evaluate ndvi") {
    interpreter((DblLit(5) - DblLit(2)) / (DblLit(5) + DblLit(2))).as[Double] match {
      case Valid(x) => x should be (0.42857 +- 0.001)
      case i@Invalid(_) => fail(s"$i")
    }
  }

  it("Should interpret and evaluate tile addition") {
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) + IntArrayTile(1 to 4 toArray, 2, 2)).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(0, 0) should be (2)
      case i@Invalid(_) => fail(s"$i")
    }
  }

  it("Should interpret and evaluate tile subtraction") {
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) - IntArrayTile(1 to 4 toArray, 2, 2)).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(0, 0) should be (0)
      case i@Invalid(_) => fail(s"$i")
    }
  }

  it("Should interpret and evaluate tile multiplication") {
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) * IntArrayTile(1 to 4 toArray, 2, 2)).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(1, 0) should be (4)
      case i@Invalid(_) => fail(s"$i")
    }
  }

  it("Should interpret and evaluate tile division") {
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) / IntArrayTile(1 to 4 toArray, 2, 2)).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(1, 0) should be (1)
      case i@Invalid(_) => fail(s"$i")
    }
  }

  it("should interpret and evaluate tile comparison") {
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) < IntArrayTile(2 to 5 toArray, 2, 2)).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(0, 0) should be (1)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) < IntArrayTile(1 to 4 toArray, 2, 2)).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(0, 0) should be (0)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) < IntArrayTile(0 to 3 toArray, 2, 2)).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(0, 0) should be (0)
      case i@Invalid(_) => fail(s"$i")
    }

    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) <= IntArrayTile(2 to 5 toArray, 2, 2)).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(0, 0) should be (1)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) <= IntArrayTile(1 to 4 toArray, 2, 2)).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(0, 0) should be (1)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) <= IntArrayTile(0 to 3 toArray, 2, 2)).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(0, 0) should be (0)
      case i@Invalid(_) => fail(s"$i")
    }

    interpreter(Equal(List(IntArrayTile(1 to 4 toArray, 2, 2), IntArrayTile(2 to 5 toArray, 2, 2)))).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(0, 0) should be (0)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(Equal(List(IntArrayTile(1 to 4 toArray, 2, 2), IntArrayTile(1 to 4 toArray, 2, 2)))).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(0, 0) should be (1)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(Equal(List(IntArrayTile(1 to 4 toArray, 2, 2), IntArrayTile(0 to 3 toArray, 2, 2)))).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(0, 0) should be (0)
      case i@Invalid(_) => fail(s"$i")
    }

    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) >= IntArrayTile(2 to 5 toArray, 2, 2)).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(0, 0) should be (0)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) >= IntArrayTile(1 to 4 toArray, 2, 2)).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(0, 0) should be (1)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) >= IntArrayTile(0 to 3 toArray, 2, 2)).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(0, 0) should be (1)
      case i@Invalid(_) => fail(s"$i")
    }

    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) > IntArrayTile(2 to 5 toArray, 2, 2)).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(0, 0) should be (0)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) > IntArrayTile(1 to 4 toArray, 2, 2)).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(0, 0) should be (0)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(IntArrayTile(1 to 4 toArray, 2, 2) > IntArrayTile(0 to 3 toArray, 2, 2)).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(0, 0) should be (1)
      case i@Invalid(_) => fail(s"$i")
    }
    interpreter(FocalSlope(List(IntArrayTile(1 to 100 toArray, 10, 10)))).as[MultibandTile] match {
      case Valid(t) => t.bands.head.get(5, 5) should be (10)
      case i@Invalid(_) => fail(s"$i")
    }
  }
}
