package com.azavea.maml.spark.eval.directive

import com.azavea.maml.eval.directive._
import com.azavea.maml.eval._
import com.azavea.maml.spark.ast._
import com.azavea.maml.spark.eval._

import cats.data._
import cats.data.Validated._


object RDDSourceDirectives {
  val rddLiteral = Directive { case (RddLit(rdd), _) => Valid(RDDResult(rdd)) }
}
