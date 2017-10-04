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
import com.typesafe.scalalogging.LazyLogging

import scala.reflect.ClassTag


sealed trait LazyTile extends LazyLogging {
  // TODO: We need to find a way to rip RasterExtent out of LazyTile
  //       while still being able to provide it to Masking operations.
  //       Is this a use case for futo or paramorphisms?
  def extent: RasterExtent
  def children: List[LazyTile]
  def cols: Int
  def rows: Int
  def get(col: Int, row: Int): Int
  def getDouble(col: Int, row: Int): Double

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

object LazyTile {

  def apply(tile: Tile, extent: Extent): LazyTile = Bound(tile, RasterExtent(extent, tile))

  def apply(tile: Tile, rasterExtent: RasterExtent): LazyTile = Bound(tile, rasterExtent)

  def apply(raster: Raster[Tile]): LazyTile = Bound(raster.tile, raster.rasterExtent)

  /** An LazyTile.Tree has a left and right. The terminal node will have Nil on the left and right */
  trait Branch extends LazyTile {
    lazy val cols = {
      val colList = children.map(_.cols).distinct
      require(colList.length == 1, "Ambiguous column count")
      colList.head
    }
    lazy val rows = {
      val rowList = children.map(_.cols).distinct
      require(rowList.length == 1, "Ambiguous row count")
      rowList.head
    }
  }

  trait UnaryBranch extends Branch {
    val arity = 1
    require(children.length == arity, s"Incorrect arity: $arity argument(s) expected, ${children.length} found")
    def fst = children.head
    def extent: RasterExtent = fst.extent
  }

  trait BinaryBranch extends Branch {
    val arity = 2
    require(children.length == arity, s"Incorrect arity: $arity argument(s) expected, ${children.length} found")
    def fst = children(0)
    def snd = children(1)
    def extent: RasterExtent = fst.extent combine snd.extent
  }

  trait Terminal extends LazyTile {
    def children: List[LazyTile]
  }

  /** This object represents tile data sources */
  case class Bound(tile: Tile, extent: RasterExtent) extends Terminal {
    def children: List[LazyTile] = List.empty
    def cols: Int = tile.cols
    def rows: Int = tile.rows
    def get(col: Int, row: Int): Int = tile.get(col,row)
    def getDouble(col: Int, row: Int): Double = tile.getDouble(col, row)
  }

  /* --- Mapping --- */
  case class MapInt(children: List[LazyTile], f: Int => Int) extends UnaryBranch {
    def get(col: Int, row: Int) = f(fst.get(col, row))
    def getDouble(col: Int, row: Int) = i2d(fst.get(col, row))
  }

  case class MapDouble(children: List[LazyTile], f: Double => Double) extends UnaryBranch {
    def get(col: Int, row: Int) = d2i(f(fst.getDouble(col, row)))
    def getDouble(col: Int, row: Int) = f(fst.getDouble(col, row))
  }

  case class DualMap(children: List[LazyTile], f: Int => Int, g: Double => Double) extends UnaryBranch {
    def get(col: Int, row: Int) = f(fst.get(col, row))
    def getDouble(col: Int, row: Int) = g(fst.getDouble(col, row))
  }

  /* --- Combining --- */
  case class CombineInt(children: List[LazyTile], f: (Int, Int) => Int) extends BinaryBranch {
    def get(col: Int, row: Int) = f(fst.get(col, row), snd.get(col, row))
    def getDouble(col: Int, row: Int) = i2d(f(fst.get(col, row), snd.get(col, row)))
  }

  case class CombineDouble(children: List[LazyTile], f: (Double, Double) => Double) extends BinaryBranch {
    def get(col: Int, row: Int) = d2i(f(fst.getDouble(col, row), snd.getDouble(col, row)))
    def getDouble(col: Int, row: Int) = f(fst.getDouble(col, row), snd.getDouble(col, row))
  }

  case class DualCombine(children: List[LazyTile], f: (Int, Int) => Int, g: (Double, Double) => Double) extends BinaryBranch {
    def get(col: Int, row: Int) = f(fst.get(col, row), snd.get(col, row))
    def getDouble(col: Int, row: Int) = g(fst.getDouble(col, row), snd.getDouble(col, row))
  }

  case class TileCombineInt(children: List[LazyTile], scalar: Int, f: (Int, Int) => Int) extends UnaryBranch {
    def get(col: Int, row: Int) = f(fst.get(col, row), scalar)
    def getDouble(col: Int, row: Int) = i2d(get(col, row))
  }

  case class TileCombineDouble(children: List[LazyTile], scalar: Double, f: (Double, Double) => Double) extends UnaryBranch {
    def get(col: Int, row: Int) = d2i(getDouble(col, row))
    def getDouble(col: Int, row: Int) = f(fst.getDouble(col, row), scalar)
  }

  case class Focal(
    children: List[LazyTile],
    neighborhood: Neighborhood,
    gridbounds: Option[GridBounds],
    focalFn: (Tile, Neighborhood, Option[GridBounds], TargetCell) => Tile
  ) extends UnaryBranch {
    override lazy val cols: Int = gridbounds.map(_.width).getOrElse(fst.cols)
    override lazy val rows: Int = gridbounds.map(_.height).getOrElse(fst.rows)
    lazy val intTile = focalFn(fst.evaluate, neighborhood, gridbounds, TargetCell.All)
    lazy val dblTile = focalFn(fst.evaluateDouble, neighborhood, gridbounds, TargetCell.All)

    def get(col: Int, row: Int) = intTile.get(col, row)
    def getDouble(col: Int, row: Int) = dblTile.get(col, row)
  }
}

