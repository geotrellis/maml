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
case class MissingParameter(i: Int) extends InterpreterError {
  def repr = s"Unbound parameter at $i encountered, unable to evaluate"
}

case class IncorrectArgCount(i: Int, expected: Int, actual: Int) extends InterpreterError {
  def repr = s"Operation ${i} was given ${actual} args, but expected ${expected}"
}

case class UnhandledCase(exp: Expression, kind: MamlKind) extends InterpreterError {
  def repr = s"A branch of Interpreter logic has yet to be implemented for the expression $exp and the kind $kind"
}

case class UnsubstitutedRef(i: Int) extends InterpreterError {
  def repr = s"Unsubstituted Tool reference found: ${i}"
}

case class NoSourceLeaves(i: Int) extends InterpreterError {
  def repr = s"The Operation ${i} has only Constant leaves"
}

case class NoBandGiven(i: Int) extends InterpreterError {
  def repr = s"No band value given for Scene ${i}"
}

case class AttributeStoreFetchError(i: Int) extends InterpreterError {
  def repr = s"Unable to fetch an S3AttributeStore for Scene ${i}"
}

case class ASTDecodeError(i: Int, msg: DecodingFailure) extends InterpreterError {
  def repr = s"Unable to decode the AST associated with ToolRun ${i}: ${msg}"
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
  implicit val encodeInterpreterErrors: Encoder[InterpreterError] =
    new Encoder[InterpreterError] {
      final def apply(err: InterpreterError): Json = JsonObject.fromMap {
        Map("node" -> err.repr.asJson, "reason" -> err.repr.asJson)
      }.asJson
    }
}

