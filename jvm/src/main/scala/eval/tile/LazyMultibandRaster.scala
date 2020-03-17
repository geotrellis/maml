package com.azavea.maml.eval.tile

import geotrellis.raster._
import geotrellis.raster.mapalgebra.focal.{Neighborhood, TargetCell}
import geotrellis.proj4.CRS
import geotrellis.vector.MultiPolygon

case class LazyMultibandRaster(bands: Map[String, LazyRaster]) {
  assert(
    Set(bands.values.map(_.crs)).size == 1,
    s"All raster data must have the same projection. Found: ${bands.values.mkString("; ")}"
  )
  def select(labels: List[String]): LazyMultibandRaster = {
    val keyOrder = labels.zipWithIndex.toMap
    val selected = bands.filterKeys { labels contains _ }
    val reordered = selected.toArray.sortWith { case ((k1, _), (k2, _)) =>
      keyOrder(k1) < keyOrder(k2)
    }.toMap
    LazyMultibandRaster(reordered)
  }

  def crs = bands.values.head.crs

  def rasterExtent = bands.values.head.rasterExtent

  def evaluateAs(ct: CellType): ProjectedRaster[MultibandTile] =
    ProjectedRaster(MultibandTile(bands.values.map(_.evaluateAs(ct))), rasterExtent.extent, crs)

  def evaluate: ProjectedRaster[MultibandTile] =
    evaluateAs(IntConstantNoDataCellType)

  def evaluateDouble: ProjectedRaster[MultibandTile] =
    evaluateAs(DoubleConstantNoDataCellType)

  def dualCombine(
    other: LazyMultibandRaster,
    f: (Int, Int) => Int,
    g: (Double, Double) => Double
  ): LazyMultibandRaster = {
    val newBands = bands.values.zip(other.bands.values).map { case (v1, v2) =>
      LazyRaster.DualCombine(List(v1, v2), f, g)
    }.toList
    LazyMultibandRaster(newBands)
  }

  def dualMap(f: Int => Int, g: Double => Double): LazyMultibandRaster =
    LazyMultibandRaster(bands.mapValues({ lt => LazyRaster.DualMap(List(lt), f, g) }))

  def focal(
    neighborhood: Neighborhood,
    gridbounds: Option[GridBounds[Int]],
    target: TargetCell,
    focalFn: (Tile, Neighborhood, Option[GridBounds[Int]], TargetCell) => Tile
  ): LazyMultibandRaster = {
    val lztiles = bands.mapValues({ lt => LazyRaster.Focal(List(lt), neighborhood, gridbounds, target, focalFn) })
    LazyMultibandRaster(lztiles)
  }

  def slope(
    gridbounds: Option[GridBounds[Int]],
    zFactor: Double,
    cs: CellSize,
    target: TargetCell
  ): LazyMultibandRaster = {
    val lztiles = bands.mapValues({ lt => LazyRaster.Slope(List(lt), gridbounds, zFactor, cs, target) })
    LazyMultibandRaster(lztiles)
  }

  def hillshade(
    gridbounds: Option[GridBounds[Int]],
    zFactor: Double,
    cs: CellSize,
    azimuth: Double,
    altitude: Double,
    target: TargetCell
  ): LazyMultibandRaster = {
    val lztiles = bands.mapValues({ lt => LazyRaster.Hillshade(List(lt), gridbounds, zFactor, cs, azimuth, altitude, target) })
    LazyMultibandRaster(lztiles)
  }

  def aspect(
    gridbounds: Option[GridBounds[Int]],
    cs: CellSize,
    target: TargetCell
  ): LazyMultibandRaster = {
    val lztiles = bands.mapValues({ lt =>
      LazyRaster.Aspect(List(lt), gridbounds, cs, target)
    })
    LazyMultibandRaster(lztiles)
  }

  def mask(
    maskPoly: MultiPolygon
  ): LazyMultibandRaster = {
    val lztiles = bands.mapValues({ lt => MaskingNode(List(lt), maskPoly) })
    LazyMultibandRaster(lztiles)
  }
}

object LazyMultibandRaster {
  def apply(lazyrasters: List[LazyRaster]): LazyMultibandRaster = {
    val defaultLabels = lazyrasters.zipWithIndex.map(lt => lt._2.toString -> lt._1).toMap
    LazyMultibandRaster(defaultLabels)
  }
  def apply(mbRaster: Raster[MultibandTile], crs: CRS): LazyMultibandRaster =
    LazyMultibandRaster(mbRaster.tile.bands.map(LazyRaster(_, mbRaster.extent, crs)).toList)

  def apply(projRaster: ProjectedRaster[MultibandTile]): LazyMultibandRaster = {
    val mbRaster = projRaster.raster
    LazyMultibandRaster(mbRaster.tile.bands.map(LazyRaster(_, mbRaster.extent, projRaster.crs)).toList)

  }
}
