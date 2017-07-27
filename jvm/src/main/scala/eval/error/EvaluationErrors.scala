package maml.eval

import maml.ast._

import io.circe._
import io.circe.syntax._


/** The type [[Interpret.Interpreted]] is either a successfully interpreted AST
  *  or else a list of all the failures the Interpreter runs into. Those errors are
  *  instances of InterpreterError.
  */
sealed trait InterpreterError {
  def repr: String
}

/** An unbound parameter encountered during evaluation  */
case class MissingParameter(i: Int) extends InterpreterError {
  def repr = s"Unbound parameter at $i encountered, unable to evaluate"
}

case class IncorrectArgCount(i: Int, expected: Int, actual: Int) extends InterpreterError {
  def repr = s"Operation ${i} was given ${actual} args, but expected ${expected}"
}

case class UnhandledCase(i: Int) extends InterpreterError {
  def repr = s"Some branch of Interpreter logic has yet to be implemented: ${i}"
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

case class TypeError(json: Json) extends InterpreterError {
  def repr = s"Type error: something"
}

case class ASTDecodeError(i: Int, msg: DecodingFailure) extends InterpreterError {
  def repr = s"Unable to decode the AST associated with ToolRun ${i}: ${msg}"
}


object InterpreterError {
  implicit val encodeInterpreterErrors: Encoder[InterpreterError] =
    new Encoder[InterpreterError] {
      final def apply(err: InterpreterError): Json = JsonObject.fromMap {
        Map("reason" -> err.repr.asJson)
      }.asJson
    }
}
