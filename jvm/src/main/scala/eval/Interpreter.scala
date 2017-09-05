package maml.eval

import maml.ast._
import maml.error._

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}

import scala.reflect.ClassTag


trait Interpreter[T] {
  def fallbackDirective: Directive[T]
  def instructions(expression: Expression[T], children: List[Result]): Interpreted[Result]
  def apply(exp: Expression[T]): Interpreted[Result] = {
    val children: Interpreted[List[Result]] = exp.children.map(apply).sequence
    children.andThen({ childRes => instructions(exp, childRes) })
  }
}

object Interpreter {
  def naive[T](directives: Directive[T]*) = NaiveInterpreter(directives.toList)

  def buffering[T](directives: ScopedDirective[BufferingInterpreter.Scope, T]*) = BufferingInterpreter(directives.toList)
}
