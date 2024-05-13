package com.azavea.maml.eval.tile

import geotrellis.proj4.WebMercator
import geotrellis.raster._
import geotrellis.layer._

/**
 * This interpreter handles resource resolution and compilation of MapAlgebra ASTs
 */
object TileLayouts {

  private val layouts: Array[LayoutDefinition] = (0 to 30).map(n => ZoomedLayoutScheme.layoutForZoom(n, WebMercator.worldExtent, 256)).toArray

  def apply(i: Int) = layouts(i)
}
