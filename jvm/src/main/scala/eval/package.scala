package maml

import maml.ast._
import maml.eval.scalar._

import cats._
import cats.data._
import cats.data.Validated._
import cats.implicits._


package object eval {
  type Interpreted[A] = ValidatedNel[InterpreterError, A]

  implicit class InterpretableExpression(val self: Expression) extends Interpretable[Expression] {
    def interpret = self match {
      case exp: ScalarSource => exp.interpret
      case exp: Addition => exp.interpret
    }
  }

  implicit class InterpretableScalar(val self: ScalarSource) extends Interpretable[ScalarSource] {
    def interpret = Scalar(self.scalar)
  }

  implicit class InterpretableAddition(val self: Addition) extends Interpretable[Addition] {
    val children = self.children.map(_.interpret)
    def interpret = {
      if (self.kind == MamlKind.Scalar) ScalarFold(children.asInstanceOf[List[ScalarRep]], _ + _, _ + _)
      else ???
    }
  }
}

