package com.azavea.maml.util

import com.azavea.maml.ast._
import com.azavea.maml.error._

import cats._
import cats.data.{NonEmptyList => NEL, _}; import Validated._
import cats.implicits._

import java.security.InvalidParameterException

object Vars {
  def vars(expr: Expression): Map[String, MamlKind] =
    varsWithBuffer(expr).map { case (name, (kind, _)) => name -> kind }

  def varsWithBuffer(expr: Expression): Map[String, (MamlKind, Int)] = {
    def eval(subExp: Expression, buffer: Int): List[(String, MamlKind, Int)] = subExp match {
      case v: Variable =>
        List((v.name, v.kind, buffer))
      case f: Expression with FocalExpression =>
        subExp.children
          .flatMap(eval(_, buffer + NeighborhoodConversion(f.neighborhood).extent))
      case _ =>
        subExp.children
          .flatMap(eval(_, buffer))
    }
    // max by the buffer to ensure that we have enough data for all operations
    eval(expr, 0)
      .groupBy(_._1)
      .mapValues { values => values.maxBy(_._3) }
      .map { case (name, (_, kind, buffer)) => name -> (kind, buffer) }
      .toMap
  }
}
