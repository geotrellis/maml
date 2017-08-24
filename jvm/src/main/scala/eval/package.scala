package maml

import maml.ast._
import maml.ast.TileSource
import maml.ast.utility._
import maml.error._
import maml.eval.tile._

import geotrellis.raster._
import geotrellis.raster.mapalgebra.focal
import cats._
import cats.data._
import cats.data.Validated._
import cats.implicits._

import scala.reflect.ClassTag


package object eval {
  type Interpreted[A] = ValidatedNel[InterpreterError, A]
  type Directive = PartialFunction[(Expression, Seq[Result]), Interpreted[Result]]
  type ScopedDirective[Scope] = PartialFunction[(Expression, Seq[Result], Scope), Interpreted[Result]]

  implicit def gtNeighborhoods(n: Neighborhood): focal.Neighborhood = n match {
    case Square(e) => focal.Square(e)
    case Circle(r) => focal.Circle(r)
    case Nesw(e) => focal.Nesw(e)
    case Wedge(r, startAngle, endAngle) => focal.Wedge(r, startAngle, endAngle)
    case Annulus(innerR, outterR) => focal.Annulus(innerR, outterR)
  }

}

