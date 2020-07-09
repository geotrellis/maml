package com.azavea.maml.eval.tile

import geotrellis.raster._
import geotrellis.raster.mapalgebra.focal.{Neighborhood, Square, TargetCell, Aspect => GTAspect, Slope => GTFocalSlope}
import geotrellis.raster.mapalgebra.focal.hillshade.{Hillshade => GTHillshade}
import geotrellis.vector.Extent
import geotrellis.proj4.CRS
import spire.syntax.cfor._

sealed trait LazyRaster {
  // TODO: We need to find a way to rip RasterExtent out of LazyRaster
  //       while still being able to provide it to Masking operations.
  //       Is this a use case for futo or paramorphisms?
  def rasterExtent: RasterExtent
  def children: List[LazyRaster]
  def cols: Int
  def rows: Int
  def get(col: Int, row: Int): Int
  def getDouble(col: Int, row: Int): Double
  def crs: CRS

  def evaluateAs(ct: CellType): Tile = {
    val mutableOutput = ArrayTile.empty(ct, cols, rows)
    if (ct.isFloatingPoint) {
      cfor(0)(_ < rows, _ + 1) { row =>
        cfor(0)(_ < cols, _ + 1) { col =>
          mutableOutput.setDouble(col, row, getDouble(col, row))
        }
      }
    } else {
      cfor(0)(_ < rows, _ + 1) { row =>
        cfor(0)(_ < cols, _ + 1) { col =>
          mutableOutput.set(col, row, get(col, row))
        }
      }
    }
    mutableOutput
  }

  def evaluate = evaluateAs(IntConstantNoDataCellType)

  def evaluateDouble = evaluateAs(DoubleConstantNoDataCellType)

}

object LazyRaster {

  def apply(tile: Tile, extent: Extent, crs: CRS): LazyRaster =
    Bound(tile, RasterExtent(extent, tile), crs)

  def apply(tile: Tile, rasterExtent: RasterExtent, crs: CRS): LazyRaster =
    Bound(tile, rasterExtent, crs)

  def apply(raster: Raster[Tile], crs: CRS): LazyRaster =
    Bound(raster.tile, raster.rasterExtent, crs)

  /** A LazyRaster.Tree has a left and right. */
  trait Branch extends LazyRaster {
    lazy val cols = {
      val colList = children.map(_.cols).distinct
      // This require block breaks things when there's an imbalance of focal operations on the children
      //require(colList.length == 1, "Ambiguous column count")
      colList.head
    }
    lazy val rows = {
      val rowList = children.map(_.rows).distinct
      // This require block breaks things when there's an imbalance of focal operations on the children
      //require(rowList.length == 1, "Ambiguous row count")
      rowList.head
    }
  }

  trait UnaryBranch extends Branch {
    val arity = 1
    require(children.length == arity, s"Incorrect arity: $arity argument(s) expected, ${children.length} found")
    def fst = children.head
    def rasterExtent: RasterExtent = fst.rasterExtent
    def crs = fst.crs
  }

  trait BinaryBranch extends Branch {
    val arity = 2
    require(children.length == arity, s"Incorrect arity: $arity argument(s) expected, ${children.length} found")
    def fst = children(0)
    def snd = children(1)
    def rasterExtent: RasterExtent = fst.rasterExtent
    def crs = fst.crs
  }

  trait Terminal extends LazyRaster {
    def children: List[LazyRaster] = List.empty
  }

  /** This object represents tile data sources */
  case class Bound(tile: Tile, rasterExtent: RasterExtent, crs: CRS) extends Terminal {
    def cols: Int = tile.cols
    def rows: Int = tile.rows
    def get(col: Int, row: Int): Int = tile.get(col,row)
    def getDouble(col: Int, row: Int): Double = tile.getDouble(col, row)
  }

  /* --- Mapping --- */
  case class MapInt(children: List[LazyRaster], f: Int => Int) extends UnaryBranch {
    def get(col: Int, row: Int) = f(fst.get(col, row))
    def getDouble(col: Int, row: Int) = i2d(fst.get(col, row))
  }

  case class MapDouble(children: List[LazyRaster], f: Double => Double) extends UnaryBranch {
    def get(col: Int, row: Int) = d2i(f(fst.getDouble(col, row)))
    def getDouble(col: Int, row: Int) = f(fst.getDouble(col, row))
  }

  case class DualMap(children: List[LazyRaster], f: Int => Int, g: Double => Double) extends UnaryBranch {
    def get(col: Int, row: Int) = f(fst.get(col, row))
    def getDouble(col: Int, row: Int) = g(fst.getDouble(col, row))
  }

  /* --- Combining --- */
  case class CombineInt(children: List[LazyRaster], f: (Int, Int) => Int) extends BinaryBranch {
    def get(col: Int, row: Int) = f(fst.get(col, row), snd.get(col, row))
    def getDouble(col: Int, row: Int) = i2d(f(fst.get(col, row), snd.get(col, row)))
  }

  case class CombineDouble(children: List[LazyRaster], f: (Double, Double) => Double) extends BinaryBranch {
    def get(col: Int, row: Int) = d2i(f(fst.getDouble(col, row), snd.getDouble(col, row)))
    def getDouble(col: Int, row: Int) = f(fst.getDouble(col, row), snd.getDouble(col, row))
  }

  case class DualCombine(children: List[LazyRaster], f: (Int, Int) => Int, g: (Double, Double) => Double) extends BinaryBranch {
    def get(col: Int, row: Int) = f(fst.get(col, row), snd.get(col, row))
    def getDouble(col: Int, row: Int) = g(fst.getDouble(col, row), snd.getDouble(col, row))
  }

  case class RasterCombineInt(children: List[LazyRaster], scalar: Int, f: (Int, Int) => Int) extends UnaryBranch {
    def get(col: Int, row: Int) = f(fst.get(col, row), scalar)
    def getDouble(col: Int, row: Int) = i2d(get(col, row))
  }

  case class RasterCombineDouble(children: List[LazyRaster], scalar: Double, f: (Double, Double) => Double) extends UnaryBranch {
    def get(col: Int, row: Int) = d2i(getDouble(col, row))
    def getDouble(col: Int, row: Int) = f(fst.getDouble(col, row), scalar)
  }

  case class Convert(
    children: List[LazyRaster],
    cellType: CellType
  ) extends UnaryBranch {
    lazy val intTile = fst.evaluate.convert(cellType)
    lazy val dblTile = fst.evaluateDouble.convert(cellType)

    def get(col: Int, row: Int) = intTile.get(col, row)
    def getDouble(col: Int, row: Int) = dblTile.getDouble(col, row)
  }

  case class InterpretAs(
    children: List[LazyRaster],
    cellType: CellType
  ) extends UnaryBranch {
    lazy val intTile = fst.evaluate.interpretAs(cellType)
    lazy val dblTile = fst.evaluateDouble.interpretAs(cellType)

    def get(col: Int, row: Int) = intTile.get(col, row)
    def getDouble(col: Int, row: Int) = dblTile.getDouble(col, row)
  }

  case class Rescale(
    children: List[LazyRaster],
    newMin: Double,
    newMax: Double
  ) extends UnaryBranch {
    lazy val intTile = fst.evaluate.rescale(newMin.toInt, newMax.toInt)
    lazy val dblTile = fst.evaluateDouble.rescale(newMin, newMax)

    def get(col: Int, row: Int) = intTile.get(col, row)
    def getDouble(col: Int, row: Int) = dblTile.getDouble(col, row)
  }

  case class Normalize(
    children: List[LazyRaster],
    oldMin: Double,
    oldMax: Double,
    newMin: Double,
    newMax: Double
  ) extends UnaryBranch {
    lazy val intTile = fst.evaluate.normalize(oldMin.toInt, oldMax.toInt, newMin.toInt, newMax.toInt)
    lazy val dblTile = fst.evaluateDouble.normalize(oldMin, oldMax, newMin, newMax)

    def get(col: Int, row: Int) = intTile.get(col, row)
    def getDouble(col: Int, row: Int) = dblTile.getDouble(col, row)
  }

  case class Clamp(
    children: List[LazyRaster],
    min: Double,
    max: Double
  ) extends UnaryBranch {
    val minInt = min.toInt
    val maxInt = max.toInt

    def clampInt(z: Int): Int =
      if(isData(z)) { if(z > maxInt) { maxInt } else if(z < minInt) { minInt } else { z } }
      else { z }

    def clampDouble(z: Double): Double =
      if(isData(z)) { if(z > max) { max } else if(z < min) { min } else { z } }
      else { z }

    lazy val intTile = fst.evaluate.map(clampInt _)
    lazy val dblTile = fst.evaluateDouble.mapDouble(clampDouble _)

    def get(col: Int, row: Int) = intTile.get(col, row)
    def getDouble(col: Int, row: Int) = dblTile.getDouble(col, row)
  }

  case class Focal(
    children: List[LazyRaster],
    neighborhood: Neighborhood,
    gridbounds: Option[GridBounds[Int]],
    target: TargetCell,
    focalFn: (Tile, Neighborhood, Option[GridBounds[Int]], TargetCell) => Tile
  ) extends UnaryBranch {
    override lazy val cols: Int = gridbounds.map(_.width).getOrElse(fst.cols)
    override lazy val rows: Int = gridbounds.map(_.height).getOrElse(fst.rows)
    lazy val intTile = focalFn(fst.evaluate, neighborhood, gridbounds, target)
    lazy val dblTile = focalFn(fst.evaluateDouble, neighborhood, gridbounds, target)

    def get(col: Int, row: Int) = intTile.get(col, row)
    def getDouble(col: Int, row: Int) = dblTile.getDouble(col, row)
  }

  case class Slope(
    children: List[LazyRaster],
    gridbounds: Option[GridBounds[Int]],
    zFactor: Double,
    cs: CellSize,
    target: TargetCell
  ) extends UnaryBranch {
    override lazy val cols: Int = gridbounds.map(_.width).getOrElse(fst.cols)
    override lazy val rows: Int = gridbounds.map(_.height).getOrElse(fst.rows)
    lazy val intTile = GTFocalSlope(fst.evaluate, Square(1), gridbounds, cs, zFactor, target)
    lazy val dblTile = GTFocalSlope(fst.evaluateDouble, Square(1),  gridbounds, cs, zFactor, target)

    def get(col: Int, row: Int) = intTile.get(col, row)
    def getDouble(col: Int, row: Int) = dblTile.getDouble(col, row)
  }

  case class Hillshade(
    children: List[LazyRaster],
    gridbounds: Option[GridBounds[Int]],
    zFactor: Double,
    cs: CellSize,
    azimuth: Double,
    altitude: Double,
    target: TargetCell
  ) extends UnaryBranch {
    override lazy val cols: Int = gridbounds.map(_.width).getOrElse(fst.cols)
    override lazy val rows: Int = gridbounds.map(_.height).getOrElse(fst.rows)

    lazy val intTile =
      GTHillshade(fst.evaluate, Square(1), gridbounds, cs, azimuth, altitude, zFactor, target)
    lazy val dblTile =
      GTHillshade(fst.evaluateDouble, Square(1), gridbounds, cs, azimuth, altitude, zFactor, target)

    def get(col: Int, row: Int) = intTile.get(col, row)
    def getDouble(col: Int, row: Int) = dblTile.getDouble(col, row)
  }

  case class Aspect(
    children: List[LazyRaster],
    gridbounds: Option[GridBounds[Int]],
    cs: CellSize,
    target: TargetCell
  ) extends UnaryBranch {
    override lazy val cols: Int = gridbounds.map(_.width).getOrElse(fst.cols)
    override lazy val rows: Int = gridbounds.map(_.height).getOrElse(fst.rows)

    lazy val intTile =
      GTAspect(fst.evaluate, Square(1), gridbounds, cs, target)
    lazy val dblTile =
      GTAspect(fst.evaluateDouble, Square(1), gridbounds, cs, target)

    def get(col: Int, row: Int) = intTile.get(col, row)
    def getDouble(col: Int, row: Int) = dblTile.getDouble(col, row)
  }
}
