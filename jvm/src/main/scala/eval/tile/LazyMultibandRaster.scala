package com.azavea.maml.eval.tile

import com.azavea.maml.eval._

import geotrellis.raster._
import geotrellis.raster.mapalgebra.local._
import geotrellis.raster.mapalgebra.focal.{ Neighborhood, TargetCell }
import geotrellis.raster.render._
import geotrellis.vector.{ Extent, MultiPolygon, Point }
import cats._
import cats.data._
import cats.data.Validated._
import cats.data.{NonEmptyList => NEL, _}
import cats.implicits._
import spire.syntax.cfor._

import scala.reflect.ClassTag


case class LazyMultibandRaster(val bands: Map[String, LazyTile]) {
  def select(labels: List[String]): LazyMultibandRaster = {
    val keyOrder = labels.zipWithIndex.toMap
    val selected = bands.filterKeys { labels contains _ }
    val reordered = selected.toArray.sortWith { case ((k1, _), (k2, _)) =>
      keyOrder(k1) < keyOrder(k2)
    }.toMap
    LazyMultibandRaster(reordered)
  }

  def evaluateAs(ct: CellType): MultibandTile =
    MultibandTile(bands.values.map(_.evaluateAs(ct)))

  def evaluate: MultibandTile =
    evaluateAs(IntConstantNoDataCellType)

  def evaluateDouble: MultibandTile =
    evaluateAs(DoubleConstantNoDataCellType)

  def dualCombine(
    other: LazyMultibandRaster,
    f: (Int, Int) => Int,
    g: (Double, Double) => Double
  ): LazyMultibandRaster = {
    val newBands = bands.values.zip(other.bands.values).map { case (v1, v2) =>
      LazyTile.DualCombine(List(v1, v2), f, g)
    }.toList
    LazyMultibandRaster(newBands)
  }

  def dualMap(f: Int => Int, g: Double => Double): LazyMultibandRaster =
    LazyMultibandRaster(bands.mapValues({ lt => LazyTile.DualMap(List(lt), f, g) }))

  def focal(
    neighborhood: Neighborhood,
    gridbounds: Option[GridBounds],
    focalFn: (Tile, Neighborhood, Option[GridBounds], TargetCell) => Tile
  ): LazyMultibandRaster = {
    val lztiles = bands.mapValues({ lt => LazyTile.Focal(List(lt), neighborhood, gridbounds, focalFn) })
    LazyMultibandRaster(lztiles)
  }
}

object LazyMultibandRaster {
  def apply(lazytiles: List[LazyTile]): LazyMultibandRaster = {
    val defaultLabels = lazytiles.zipWithIndex.map(lt => lt._2.toString -> lt._1).toMap
    LazyMultibandRaster(defaultLabels)
  }
  def apply(mbRaster: Raster[MultibandTile]): LazyMultibandRaster =
    LazyMultibandRaster(mbRaster.tile.bands.map(LazyTile(_, mbRaster.extent)).toList)
}
