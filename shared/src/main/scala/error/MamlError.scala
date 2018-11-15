package com.azavea.maml.error

import com.azavea.maml.ast._

import io.circe._
import io.circe.syntax._


/** Custom, MAML-specific errors */
trait MamlError {
  def repr: String
}


object MamlError {
  implicit val encodeMamlError: Encoder[MamlError] =
    Encoder.encodeString.contramap[MamlError](_.repr)
}

/** Error to which signifies that a nodes aregument count is incorrect */
case class IncorrectArgCount(exp: Expression, expectedArgs: Int) extends MamlError {
  def repr = s"Expected $expectedArgs arguments to ${exp}; instead, found ${exp.children.size}"
}

/** Error to use when an unhandled node is encountered during evaluation  */
case class UnhandledCase(exp: Expression, kind: MamlKind) extends MamlError {
  def repr = s"A branch of Interpreter logic has yet to be implemented for the expression ${exp} and the kind $kind"
}

case class ASTParseError(json: String, reason: String) extends MamlError {
  def repr = s"Unable to parse ${json} as JSON: ${reason}"
}

case class ASTDecodeError(json: Json, reason: String) extends MamlError {
  def repr = s"Unable to decode the json ${json} as AST: ${reason}"
}

case class DivergingTypes(found: String, expected: List[String]) extends MamlError {
  def repr: String = s"Expected to evaluate tree as one of $expected; instead found $found"
}

case class NoVariableBinding(variable: Variable, bindings: Map[String, Literal]) extends MamlError {
  def repr: String = s"No binding for ${variable.name} found in ${bindings.keys.toList}"
}

case class BindingUndefined(exp: Expression) extends MamlError {
  def repr: String = s"No logic defined to bind literal value to tree for $exp"
}

case class NonEvaluableNode(exp: Expression, reason: Option[String]) extends MamlError {
  def repr: String = reason match {
    case Some(r) =>
      s"Unable to evaluate $exp due to $r"
    case None =>
      s"Unable to evaluate $exp due to an unknown reason"
  }
}
