package com.azavea.maml.spark.eval.directive

import cats.data._
import cats.data.Validated._
import com.azavea.maml.eval.directive._
import com.azavea.maml.spark.ast._
import com.azavea.maml.spark.eval._


object RDDSourceDirectives {
  val rddLiteral = Directive { case (RDDLiteral(rdd), _) => Valid(RDDResult(rdd)) }
}
