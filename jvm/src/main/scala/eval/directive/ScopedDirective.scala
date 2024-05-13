package com.azavea.maml.eval.directive

import com.azavea.maml.ast._
import com.azavea.maml.eval._
import com.azavea.maml.error._

import cats._
import cats.data._
import cats.data.Validated._
import cats.implicits._

import scala.reflect.ClassTag

object ScopedDirective {

  /**
   * Lift a simple directive into a scoped context
   */
  def pure[Exp <: Expression: ClassTag](ruleFn: Directive): ScopedDirective[Any] = { case (exp: Exp, results: Seq[Result], _: Any) =>
    ruleFn(exp, results)
  }

  def apply[Scope](ruleFn: PartialFunction[(Expression, Seq[Result], Scope), Interpreted[Result]]): ScopedDirective[Scope] =
    ruleFn
}
