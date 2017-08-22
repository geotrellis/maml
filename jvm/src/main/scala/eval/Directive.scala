package maml.eval

import maml.ast._

import cats._
import cats.data._
import cats.data.Validated._
import cats.implicits._

import scala.reflect.ClassTag


object Directive {
  def apply(ruleFn: PartialFunction[(Expression, Seq[Result]), Interpreted[Result]]): Directive = ruleFn
}

object ScopedDirective {
  /** Lift a simple directive into a scoped context */
  def pure[Exp <: Expression : ClassTag](ruleFn: Directive): ScopedDirective[Any] =
    { case (exp: Exp, results: Seq[Result], _: Any) => ruleFn(exp, results) }

  def apply[Scope](ruleFn: PartialFunction[(Expression, Seq[Result], Scope), Interpreted[Result]]): ScopedDirective[Scope] =
    ruleFn
}
