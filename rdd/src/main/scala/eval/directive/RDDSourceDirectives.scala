package maml.rdd.eval.directive

import maml.eval.directive._
import maml.rdd.ast._
import maml.rdd.eval._

import cats.data._
import Validated._


object RDDSourceDirectives {
  val spatialRDDLiteralDirective = Directive { case (SpatialRDDLiteral(rdd), _) => Valid(SpatialRDDResult(rdd)) }
}
