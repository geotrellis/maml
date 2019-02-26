package com.azavea.maml.eval.tile

import geotrellis.raster._
import geotrellis.raster.mapalgebra.local._
import geotrellis.raster.mapalgebra.focal
import geotrellis.raster.mapalgebra.focal.Neighborhood
import geotrellis.raster.render._
import geotrellis.vector.{ Extent, MultiPolygon, Point }
import spire.syntax.cfor._


case class MaskingNode(children: List[LazyRaster], mask: MultiPolygon) extends LazyRaster.UnaryBranch {
  lazy val cellMask: Tile = {
    val masky = ArrayTile.empty(BitCellType, this.cols, this.rows)

    rasterExtent
      .foreach(mask)({ (col, row) => masky.set(col, row, 1) })

    masky
  }

  /** Perform the NODATA checks ahead of time, in case the underlying Tile
    * is sparse. This will then only check for Mask intersection if the value to
    * give back could be something other than NODATA.
    */
  def get(col: Int, row: Int): Int = {
    val v: Int = fst.get(col, row)

    if (isNoData(v)) v else if (cellMask.get(col, row) == 1) v else NODATA
  }
  def getDouble(col: Int, row: Int): Double = {
    val v: Double = fst.getDouble(col, row)

    if (isNoData(v)) v else if (cellMask.get(col, row) == 1) v else Double.NaN
  }
}

