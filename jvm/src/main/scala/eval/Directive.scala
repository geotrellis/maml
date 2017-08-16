package maml.eval

import maml.ast._

import cats._
import cats.data._
import cats.data.Validated._
import cats.implicits._


object Directive {
  def apply(ruleFn: PartialFunction[(Expression, Seq[Result]), Interpreted[Result]]): Directive = ruleFn
}

