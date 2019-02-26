package com.azavea.maml.dsl

import com.azavea.maml.eval.tile._


package object tile {
  implicit class LazyRasterExtensions(val self: LazyRaster) extends LazyRasterOperations
  implicit class LazyMultibandRasterExtensions(val self: LazyMultibandRaster) extends LazyMultibandRasterOperations
}
