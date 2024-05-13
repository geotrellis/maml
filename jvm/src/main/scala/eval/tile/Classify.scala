package com.azavea.maml.eval.tile

import geotrellis.raster._

case class Classify(children: List[LazyRaster], f: Double => Int) extends LazyRaster.UnaryBranch {
  def get(col: Int, row: Int) = f(fst.getDouble(col, row))
  def getDouble(col: Int, row: Int) = i2d(get(col, row))
}
