package maml.eval

import maml.ast._
import maml.eval.tile._
import maml.eval.scalar.Scalar

import cats._
import cats.data._
import cats.data.Validated._
import cats.implicits._
import geotrellis.raster._


trait Interpretable[A <: Expression] extends Serializable {
  def self: A
  def interpret: LazyRep
}

