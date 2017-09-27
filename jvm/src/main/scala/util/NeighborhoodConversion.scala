package com.azavea.maml.util

import geotrellis.raster.mapalgebra.focal


object NeighborhoodConversion {
  def apply(mamlN: Neighborhood): focal.Neighborhood = mamlN match {
    case Square(extent) => focal.Square(extent)
    case Circle(radius) => focal.Circle(radius)
    case Nesw(extent) => focal.Nesw(extent)
    case Wedge(radius, startAngle, endAngle) => focal.Wedge(radius, startAngle, endAngle)
    case Annulus(innerRadius, outerRadius) => focal.Annulus(innerRadius, outerRadius)
  }
}
