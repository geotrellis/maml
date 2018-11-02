package com.azavea.maml.dsl

import com.azavea.maml.eval.tile._


package object tile {
  implicit class LazyTileExtensions(val self: LazyTile) extends LazyTileOperations
  implicit class LazyMultibandRasterExtensions(val self: LazyMultibandRaster) extends LazyMultibandRasterOperations
}
