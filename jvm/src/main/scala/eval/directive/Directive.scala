package maml.eval.directive

import maml.ast._
import maml.eval._

import cats._
import cats.data._
import cats.data.Validated._
import cats.implicits._

import scala.reflect.ClassTag


object Directive {
  def apply(ruleFn: PartialFunction[(Expression, Seq[Result]), Interpreted[Result]]): Directive = ruleFn
}

