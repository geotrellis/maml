package maml.eval.directive

import maml.ast._
import maml.eval._

import cats._
import cats.data._
import cats.data.Validated._
import cats.implicits._

import scala.reflect.ClassTag


object ScopedDirective {
  // TODO Should this be `Any` or a type variable?
  /** Lift a simple directive into a scoped context */
  def pure[Exp <: Expression[T] : ClassTag, T](ruleFn: Directive[T]): ScopedDirective[Any, T] =
    { case (exp: Exp, results: Seq[Result], _: Any) => ruleFn(exp, results) }

  def apply[Scope, T](ruleFn: PartialFunction[(Expression[T], Seq[Result], Scope), Interpreted[Result]]): ScopedDirective[Scope, T] =
    ruleFn
}
