package maml.error

import maml.ast._

import io.circe._
import io.circe.syntax._


/** The type [[Interpret.Interpreted]] is either a successfully interpreted AST
  *  or else a list of all the failures the Interpreter runs into. Those errors are
  *  instances of InterpreterError.
  */
trait InterpreterError {
  def repr: String
}

/** An unbound parameter encountered during evaluation  */
case class UnhandledCase(exp: Expression, kind: MamlKind) extends InterpreterError {
  def repr = s"A branch of Interpreter logic has yet to be implemented for the expression $exp and the kind $kind"
}

case class ASTDecodeError(json: Json, msg: DecodingFailure) extends InterpreterError {
  def repr = s"Unable to decode the json ${json} as AST: ${msg}"
}

/* --- Type Errors --- */
sealed trait TypeError extends InterpreterError {}

case class UnaryTypeError(nodeType: UnaryExpression, found: MamlKind) extends TypeError {
  def repr = s"TypeError: invalid argument type $found for $nodeType"
}

case class FoldableTypeError(nodeType: FoldableExpression, found: (MamlKind, MamlKind)) extends TypeError {
  def repr = s"TypeError: unable to determine type of $nodeType for arguments: $found"
}

object InterpreterError {
  implicit val encodeInterpreterError: Encoder[InterpreterError] =
    Encoder.encodeString.contramap[InterpreterError](_.repr)
}

