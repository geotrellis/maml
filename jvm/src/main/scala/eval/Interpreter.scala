package com.azavea.maml.eval

import com.azavea.maml.ast._
import com.azavea.maml.error._

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}

import scala.reflect.ClassTag


trait Interpreter {
  def apply(exp: Expression): Interpreted[Result]
}

object Interpreter {
  val DEFAULT = NaiveInterpreter.DEFAULT
}

