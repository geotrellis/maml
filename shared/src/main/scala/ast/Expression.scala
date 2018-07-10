package com.azavea.maml.ast

import com.azavea.maml.error._
import com.azavea.maml.eval.Interpreted
import com.azavea.maml.util.NeighborhoodConversion

import cats._
import cats.data.{NonEmptyList => NEL, _}; import Validated._
import cats.implicits._
import io.circe.generic.JsonCodec


/** The ur-type for a recursive representation of MapAlgebra operations */
trait Expression extends Product with Serializable {
  def children: List[Expression]
  def kind: MamlKind
  def withChildren(newChildren: List[Expression]): Expression

  def bind(args: Map[String, Literal]): Interpreted[Expression] =
    children.map(_.bind(args)).sequence.map(this.withChildren)
}

object Expression {
  def vars(expr: Expression): Map[String, MamlKind] =
    varsWithBuffer(expr).map { case (name, (kind, _)) => name -> kind }

  def varsWithBuffer(expr: Expression): Map[String, (MamlKind, Int)] = {
    def eval(subExp: Expression, buffer: Int): List[(String, MamlKind, Int)] = subExp match {
      case v: Variable =>
        List((v.name, v.kind, buffer))
      case s: Source =>
        List()
      case f: FocalExpression =>
        subExp.children
          .flatMap(eval(_, buffer + NeighborhoodConversion(f.neighborhood).extent))
      case _ =>
        subExp.children
          .flatMap(eval(_, buffer))
    }
    // max by the buffer to ensure that we have enough data for all operations
    eval(expr, 0)
      .groupBy(_._1)
      .mapValues({ values => values.maxBy(_._3) })
      .map({ case (name, (_, kind, buffer)) => name -> (kind, buffer) })
      .toMap
  }

  def bindParams(expr: Expression, params: Map[String, Literal]): Interpreted[Expression] = {
    def eval(subExpr: Expression): Interpreted[Expression] = subExpr match {
      case v: Variable =>
        params.get(v.name) match {
          case Some(literal) if literal.kind == subExpr.kind => Valid(literal)
          case Some(literal) => Invalid(NEL.of(DivergingTypes(literal.kind.toString, List(subExpr.kind.toString))))
          case None => Invalid(NEL.of(NoVariableBinding(v, params)))
        }
      case _ =>
        subExpr.children.map(eval(_)).sequence.map(subExpr.withChildren)
    }
    eval(expr)
  }


}
