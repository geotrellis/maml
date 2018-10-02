package com.azavea.maml.ast

import com.azavea.maml.error._

import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._
import io.circe.Json
import io.circe.generic.JsonCodec

import java.lang.IllegalArgumentException


trait Source

trait Literal extends Source { expression: Expression =>
  val children: List[Expression] = List.empty
  def withChildren(children: List[Expression]) = expression
}

trait Variable extends Source { expression: Expression =>
  def name: String
  val children: List[Expression] = List.empty
  def withChildren(children: List[Expression]) = expression
}

