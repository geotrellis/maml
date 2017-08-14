package maml.eval

import maml.ast._
import maml.ast.MamlKind._
import maml.error._

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}

import scala.reflect.ClassTag


trait Interpreter {
  type SrcTransform = Source => Result
  type OpTransform = List[Result] => Result

  sealed trait Result {
    def as[T: ClassTag]: Interpreted[T]
    def kind: MamlKind
  }

  var sources: Map[Expression, SrcTransform] = Map()
  def source(src: Source)(f: SrcTransform): Unit =
    { sources = sources updated (src, f) }
  def lookupSrc(src: Source): Interpreted[Result] =
    sources.get(src) match {
      case Some(f) => Valid(f(src))
      case None => Invalid(NEL.of(UnhandledCase(src, src.kind)))
    }


  var operations: Map[(Operation, MamlKind), List[Result] => Result] = Map()
  def operation(op: Operation, kind: MamlKind)(f: OpTransform): Unit =
    { operations = operations updated (op -> kind, f) }
  def lookupOp(op: Operation, kind: MamlKind): Interpreted[OpTransform] =
    operations.get(op -> kind) match {
      case Some(func) => Valid(func)
      case None => Invalid(NEL.of(UnhandledCase(op, kind)))
    }

  def evaluate(exp: Expression): Interpreted[Result] = {
    val kind: MamlKind = exp.kind
    exp match {
      case op: Operation =>
        val childrenResult = op.children.map(evaluate).sequence
        (lookupOp(op, kind) |@| childrenResult).map({ case (f, childResults) => f(childResults) })
      case src: Source =>
        lookupSrc(src)
    }
  }
}

