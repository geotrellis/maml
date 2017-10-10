package com.azavea.maml.spark.eval

import com.azavea.maml.ast._
import com.azavea.maml.eval._
import com.azavea.maml.eval.directive._
import com.azavea.maml.spark.eval.directive._

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}

import scala.reflect.ClassTag


object RDDInterpreter {

  def DEFAULT = NaiveInterpreter(
    List(
      RDDSourceDirectives.rddLiteral,
      SourceDirectives.intLiteral,
      SourceDirectives.dblLiteral,
      SourceDirectives.boolLiteral,
      SourceDirectives.geomJson,
      RDDOpDirectives.addition,
      RDDOpDirectives.subtraction,
      RDDOpDirectives.multiplication,
      RDDOpDirectives.division,
      RDDOpDirectives.max,
      RDDOpDirectives.min,
      RDDOpDirectives.or,
      RDDOpDirectives.and,
      RDDOpDirectives.xor,
      RDDOpDirectives.equalTo,
      RDDOpDirectives.unequalTo,
      RDDOpDirectives.lessThan,
      RDDOpDirectives.lessThanOrEqualTo,
      RDDOpDirectives.greaterThan,
      RDDOpDirectives.greaterThanOrEqualTo,
      RDDOpDirectives.masking,
      RDDOpDirectives.atan2,
      RDDOpDirectives.pow,
      RDDOpDirectives.classify,
      RDDOpDirectives.sin,
      RDDOpDirectives.cos,
      RDDOpDirectives.tan,
      RDDOpDirectives.sinh,
      RDDOpDirectives.cosh,
      RDDOpDirectives.tanh,
      RDDOpDirectives.asin,
      RDDOpDirectives.acos,
      RDDOpDirectives.atan,
      RDDOpDirectives.floor,
      RDDOpDirectives.ceil,
      RDDOpDirectives.loge,
      RDDOpDirectives.log10,
      RDDOpDirectives.root,
      RDDOpDirectives.round,
      RDDOpDirectives.abs,
      RDDOpDirectives.defined,
      RDDOpDirectives.undefined,
      RDDOpDirectives.numNegation,
      RDDOpDirectives.logNegation,
      RDDOpDirectives.focalMax,
      RDDOpDirectives.focalMin,
      RDDOpDirectives.focalMean,
      RDDOpDirectives.focalMedian,
      RDDOpDirectives.focalMode,
      RDDOpDirectives.focalSum,
      RDDOpDirectives.focalStdDev
    )
  )
}
