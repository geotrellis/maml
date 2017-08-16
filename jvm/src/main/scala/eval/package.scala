package maml

import maml.ast._
import maml.ast.TileSource
import maml.error._
import maml.eval.tile._

import geotrellis.raster._
import cats._
import cats.data._
import cats.data.Validated._
import cats.implicits._

import scala.reflect.ClassTag


package object eval {
  type Interpreted[A] = ValidatedNel[InterpreterError, A]
  type Directive = PartialFunction[(Expression, Seq[Result]), Interpreted[Result]]

  implicit class TypeRefinement(self: Interpreted[Result]) {
    def as[T](implicit ct: ClassTag[T]): Interpreted[T] = self match {
      case Valid(r) => r.as[T](ct)
      case i@Invalid(_) => i
    }
  }
}

