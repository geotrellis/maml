package com.azavea.maml.spark.eval.directive

import com.azavea.maml.eval.directive._
import com.azavea.maml.spark.ast._
import com.azavea.maml.spark.eval._

import cats.data._
import Validated._


object RDDSourceDirectives {
  val spatialRDDLiteralDirective = Directive { case (SpatialRDDLiteral(rdd), _) => Valid(SpatialRDDResult(rdd)) }
}
