package com.azavea.maml.eval

import com.azavea.maml.ast._

import io.circe._
import io.circe.syntax._


/** The type [[Interpret.Interpreted]] is either a successfully interpreted AST
  *  or else a list of all the failures the Interpreter runs into. Those errors are
  *  instances of InterpreterError.
  */
trait InterpreterError {
  def repr: String
}


object InterpreterError {
  implicit val encodeInterpreterError: Encoder[InterpreterError] =
    Encoder.encodeString.contramap[InterpreterError](_.repr)
}

/** Error to which signifies that a nodes aregument count is incorrect */
case class IncorrectArgCount(exp: Expression, expectedArgs: Int) extends InterpreterError {
  def repr = s"Expected $expectedArgs arguments to ${exp}; instead, found ${exp.children.size}"
}

/** Error to use when an unhandled node is encountered during evaluation  */
case class UnhandledCase(exp: Expression, kind: MamlKind) extends InterpreterError {
  def repr = s"A branch of Interpreter logic has yet to be implemented for the expression ${exp} and the kind $kind"
}

case class ASTDecodeError(json: Json, msg: DecodingFailure) extends InterpreterError {
  def repr = s"Unable to decode the json ${json} as AST: ${msg}"
}

case class EvalTypeError(found: String, expected: List[String]) extends InterpreterError {
  def repr: String = s"Expected to evaluate tree as one of $expected; instead found $found"
}

case class S3TileResolutionError(exp: Expression, coords: Option[(Int, Int, Int)]) extends InterpreterError {
  def repr: String = coords match {
    case Some((z, x, y)) => s"Tile not found for ${exp} at SpatialKey $z, $x, $y}"
    case None => s"Tile not found for ${exp}"
  }
}

case class UnknownTileResolutionError(exp: Expression, coords: Option[(Int, Int, Int)]) extends InterpreterError {
  def repr: String = coords match {
    case Some((z, x, y)) => s"Unknown retrieval error for ${exp} at SpatialKey $z, $x, $y}"
    case None => s"Unkown retrieval error for ${exp}"
  }
}

case class NonEvaluableNode(exp: Expression) extends InterpreterError {
  def repr: String = s"Encountered non-evaluable node: $exp"
}
