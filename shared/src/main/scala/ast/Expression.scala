package com.azavea.maml.ast

import com.azavea.maml.eval.Interpreted

import cats._
import cats.implicits._
import io.circe.generic.JsonCodec


/** The ur-type for a recursive representation of MapAlgebra operations */
trait Expression extends Product with Serializable {
  def children: List[Expression]
  def kind: MamlKind
  def withChildren(newChildren: List[Expression]): Expression

  def varMap: Map[String, MamlKind] =
    children
      .map(_.varMap)
      .foldLeft(Map[String, MamlKind]())(_ ++ _)

  def bind(args: Map[String, Literal]): Interpreted[Expression] =
    children.map(_.bind(args)).sequence.map(this.withChildren)
}

