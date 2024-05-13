package com.azavea.maml

import com.azavea.maml.ast._
import com.azavea.maml.error._
import com.azavea.maml.util._
import com.azavea.maml.eval.tile._

import geotrellis.raster._
import geotrellis.raster.mapalgebra.focal
import cats._
import cats.data._
import cats.data.Validated._
import cats.implicits._

package object eval {
  type Directive = PartialFunction[(Expression, Seq[Result]), Interpreted[Result]]
  type ScopedDirective[Scope] = PartialFunction[(Expression, Seq[Result], Scope), Interpreted[Result]]
}
