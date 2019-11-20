package com.azavea.maml.eval

import com.azavea.maml.eval.tile._
import com.azavea.maml.eval._
import com.azavea.maml.error._
import com.azavea.maml.ast._

import cats.data.{NonEmptyList => NEL, _}
import cats.data.Validated._
import geotrellis.raster._
import geotrellis.spark._
import geotrellis.layer._

import scala.reflect.ClassTag


case class RDDResult(res: TileLayerRDD[SpatialKey]) extends Result {
  def as[T](implicit ct: ClassTag[T]): Interpreted[T] = {
    val cls = ct.runtimeClass
    if (classOf[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]] isAssignableFrom cls)
      Valid(res.asInstanceOf[T])
    else
      Invalid(NEL.of(DivergingTypes(cls.getName, List("SpatialRDD"))))
  }
  def kind: MamlKind = MamlKind.Image
}
