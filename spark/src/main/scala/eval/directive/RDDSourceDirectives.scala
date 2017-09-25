package maml.spark.eval.directive

import maml.eval.directive._
import maml.spark.ast._
import maml.spark.eval._

import cats.data._
import Validated._


object RDDSourceDirectives {
  val spatialRDDLiteralDirective = Directive { case (SpatialRDDLiteral(rdd), _) => Valid(SpatialRDDResult(rdd)) }
}
