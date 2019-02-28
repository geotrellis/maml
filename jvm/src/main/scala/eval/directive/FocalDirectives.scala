package com.azavea.maml.eval.directive

import com.azavea.maml.eval._
import com.azavea.maml.error._
import com.azavea.maml.ast._
import com.azavea.maml.eval.tile._
import com.azavea.maml.util._

import geotrellis.raster.Tile
import geotrellis.raster.mapalgebra.focal
import geotrellis.vector.Point
import geotrellis.proj4.{CRS, LatLng}
import cats._
import cats.implicits._
import cats.data.{NonEmptyList => NEL, _}
import Validated._


object FocalDirectives {
  val max = Directive { case (fm@FocalMax(_, neighborhood), childResults) =>
    childResults
      .map({ _.as[LazyMultibandRaster] })
      .toList.sequence
      .map({ lr =>
        ImageResult(lr.head.focal(NeighborhoodConversion(neighborhood), None, focal.Max.apply _))
      })
  }

  val min = Directive { case (fm@FocalMin(_, neighborhood), childResults) =>
    childResults
      .map({ _.as[LazyMultibandRaster] })
      .toList.sequence
      .map({ lr =>
        ImageResult(lr.head.focal(NeighborhoodConversion(neighborhood), None, focal.Min.apply _))
      })
  }

  val mean = Directive { case (fm@FocalMean(_, neighborhood), childResults) =>
    childResults
      .map({ _.as[LazyMultibandRaster] })
      .toList.sequence
      .map({ lr =>
        ImageResult(lr.head.focal(NeighborhoodConversion(neighborhood), None, focal.Mean.apply _))
      })
  }

  val median = Directive { case (fm@FocalMedian(_, neighborhood), childResults) =>
    childResults
      .map({ _.as[LazyMultibandRaster] })
      .toList.sequence
      .map({ lr =>
        ImageResult(lr.head.focal(NeighborhoodConversion(neighborhood), None, focal.Median.apply _))
      })
  }

  val mode = Directive { case (fm@FocalMode(_, neighborhood), childResults) =>
    childResults
      .map({ _.as[LazyMultibandRaster] })
      .toList.sequence
      .map({ lr =>
        ImageResult(lr.head.focal(NeighborhoodConversion(neighborhood), None, focal.Mode.apply _))
      })
  }

  val sum = Directive { case (fm@FocalSum(_, neighborhood), childResults) =>
    childResults
      .map({ _.as[LazyMultibandRaster] })
      .toList.sequence
      .map({ lr =>
        ImageResult(lr.head.focal(NeighborhoodConversion(neighborhood), None, focal.Sum.apply _))
      })
  }

  val standardDeviation = Directive { case (fm@FocalStdDev(_, neighborhood), childResults) =>
    childResults
      .map({ _.as[LazyMultibandRaster] })
      .toList.sequence
      .map({ lr =>
        ImageResult(lr.head.focal(NeighborhoodConversion(neighborhood), None, focal.StandardDeviation.apply _))
      })
  }

  val slope = Directive { case (fm@FocalSlope(_), childResults) =>
    childResults
      .map({ _.as[LazyMultibandRaster] })
      .toList.sequence
      .map({ lr =>
        val image = lr.head
        val re = image.rasterExtent
        val zfactor = {
          val llExtent = re.extent.reproject(image.crs, LatLng)
          val middleY = llExtent.ymax - (llExtent.ymax - llExtent.ymin)
          val EQUATOR_METERS = 11320
          1 / (EQUATOR_METERS * math.cos(math.toRadians(middleY)))
        }
        println(image.slope(None, zfactor, re.cellSize))
        ImageResult(image.slope(None, zfactor, re.cellSize))
      })
  }
}
