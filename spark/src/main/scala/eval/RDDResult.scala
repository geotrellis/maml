package com.azavea.maml.spark.eval

import scala.reflect.ClassTag

import cats.data.{NonEmptyList => NEL, _}
import cats.data.Validated._
import com.azavea.maml.ast._
import com.azavea.maml.eval._
import geotrellis.raster.Tile
import geotrellis.spark._


case class RDDResult(res: TileLayerRDD[SpatialKey]) extends Result {
  def as[T](implicit ct: ClassTag[T]): Interpreted[T] = {
    val cls = ct.runtimeClass
    if (classOf[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]] isAssignableFrom cls)
      Valid(res.asInstanceOf[T])
    else
      Invalid(NEL.of(EvalTypeError(cls.getName, List("SpatialRDD"))))
  }
  def kind: MamlKind = MamlKind.Tile
}
