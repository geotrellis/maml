package com.azavea.maml.eval

import com.azavea.maml.ast._
import com.azavea.maml.eval.directive._

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}

import scala.reflect.ClassTag


case class NaiveInterpreter(directives: List[Directive]) extends Interpreter {

  def prependDirective(directive: Directive): Interpreter =
    NaiveInterpreter(directive +: directives)

  def appendDirective(directive: Directive): Interpreter =
    NaiveInterpreter(directives :+ directive)

  val fallbackDirective: Directive =
    { case (exp, res) => Invalid(NEL.of(UnhandledCase(exp, exp.kind))) }

  def instructions(expression: Expression, children: List[Result]): Interpreted[Result] =
    directives.reduceLeft(_ orElse _).orElse(fallbackDirective)((expression, children))
}

object NaiveInterpreter {

  def DEFAULT = NaiveInterpreter(
    List(
      SourceDirectives.tileLiteral,
      SourceDirectives.intLiteral,
      SourceDirectives.dblLiteral,
      SourceDirectives.boolLiteral,
      SourceDirectives.geomJson,
      OpDirectives.additionTile orElse OpDirectives.additionInt orElse OpDirectives.additionDouble,
      OpDirectives.subtraction,
      OpDirectives.multiplicationTile orElse OpDirectives.multiplicationInt orElse OpDirectives.multiplicationDouble,
      OpDirectives.division,
      OpDirectives.maxTile orElse OpDirectives.maxInt orElse OpDirectives.maxDouble,
      OpDirectives.minTile orElse OpDirectives.minInt orElse OpDirectives.minDouble,
      OpDirectives.lessThan,
      OpDirectives.lessThanOrEqualTo,
      OpDirectives.equalTo,
      OpDirectives.notEqualTo,
      OpDirectives.greaterThan,
      OpDirectives.greaterThanOrEqualTo,
      OpDirectives.and,
      OpDirectives.or,
      OpDirectives.xor,
      OpDirectives.masking,
      OpDirectives.atan2,
      UnaryDirectives.sin,
      UnaryDirectives.cos,
      UnaryDirectives.tan,
      UnaryDirectives.sinh,
      UnaryDirectives.cosh,
      UnaryDirectives.tanh,
      UnaryDirectives.asin,
      UnaryDirectives.acos,
      UnaryDirectives.atan,
      UnaryDirectives.round,
      UnaryDirectives.floor,
      UnaryDirectives.ceil,
      UnaryDirectives.naturalLog,
      UnaryDirectives.log10,
      UnaryDirectives.sqrt,
      UnaryDirectives.abs,
      UnaryDirectives.isUndefined,
      UnaryDirectives.isDefined,
      UnaryDirectives.numericNegation,
      UnaryDirectives.logicalNegation,
      UnaryDirectives.classification,
      FocalDirectives.max,
      FocalDirectives.min,
      FocalDirectives.mean,
      FocalDirectives.mode,
      FocalDirectives.median,
      FocalDirectives.sum,
      FocalDirectives.standardDeviation
    )
  )
}
