package maml.error

import maml.ast._

import io.circe._
import io.circe.syntax._


/** The type [[Interpreter.Interpreted]] is either a successfully interpreted AST
  *  or else a list of all the failures the Interpreter runs into. Those errors are
  *  instances of InterpreterError.
  */
sealed trait InterpreterError {
  def repr: String
}

/* --- Type Errors --- */
sealed trait TypeError extends InterpreterError {}

case class UnaryTypeError(nodeType: UnaryExpression, found: MamlKind) extends TypeError {
  def repr = s"TypeError: invalid argument type $found for $nodeType"
}

case class FoldableTypeError(nodeType: FoldableExpression, found: (MamlKind, MamlKind)) extends TypeError {
  def repr = s"TypeError: unable to determine type of $nodeType for arguments: $found"
}

/** An unbound parameter encountered during evaluation  */
//case class MissingParameter(i: Int) extends InterpreterError {
//  def repr = s"Unbound parameter at $i encountered, unable to evaluate"
//}

//case class IncorrectArgCount(id: UUID, expected: Int, actual: Int) extends InterpreterError {
//  def repr = s"Operation ${id} was given ${actual} args, but expected ${expected}"
//}

//case class UnhandledCase(id: UUID) extends InterpreterError {
//  def repr = s"Some branch of Interpreter logic has yet to be implemented: ${id}"
//}

//case class UnsubstitutedRef(id: UUID) extends InterpreterError {
//  def repr = s"Unsubstituted Tool reference found: ${id}"
//}

//case class NoSourceLeaves(id: UUID) extends InterpreterError {
//  def repr = s"The Operation ${id} has only Constant leaves"
//}

//case class NoBandGiven(id: UUID) extends InterpreterError {
//  def repr = s"No band value given for Scene ${id}"
//}

//case class AttributeStoreFetchError(id: UUID) extends InterpreterError {
//  def repr = s"Unable to fetch an S3AttributeStore for Scene ${id}"
//}

///** An error encountered when a bound parameter's source can't be resolved */
//case class RasterRetrievalError(id: UUID, refId: UUID) extends InterpreterError {
//  def repr = s"Unable to retrieve raster for ${refId} on AST node ${id}"
//}

//case class DatabaseError(id: UUID) extends InterpreterError {
//  def repr = s"Unable to retrieve ToolRun $id or its associated Tool from the database"
//}

//case class ASTDecodeError(id: UUID, msg: DecodingFailure) extends InterpreterError {
//  def repr = s"Unable to decode the AST associated with ToolRun ${id}: ${msg}"
//}

//case class InvalidOverride(id: UUID) extends InterpreterError {
//  def repr = s"Node ${id} was given an incompatible override value"
//}

object InterpreterError {
  implicit val encodeInterpreterErrors: Encoder[InterpreterError] =
    new Encoder[InterpreterError] {
      final def apply(err: InterpreterError): Json = JsonObject.fromMap {
        Map("node" -> err.repr.asJson, "reason" -> err.repr.asJson)
      }.asJson
    }
}
