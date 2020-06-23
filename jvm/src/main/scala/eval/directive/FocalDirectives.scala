package com.azavea.maml.eval.directive

import com.azavea.maml.eval._
import com.azavea.maml.error._
import com.azavea.maml.ast._
import com.azavea.maml.eval.tile._
import com.azavea.maml.util._

import geotrellis.raster.Tile
import geotrellis.raster.mapalgebra.focal
import geotrellis.vector.Point
import geotrellis.proj4.LatLng

import cats.implicits._
import cats.data._
import Validated._


object FocalDirectives {
  val max = Directive { case (FocalMax(_, neighborhood, target), childResults) =>
    childResults
      .toList
      .traverse { _.as[LazyMultibandRaster] }
      .map({ lr =>
        ImageResult(lr.head.focal(NeighborhoodConversion(neighborhood), None, target, focal.Max.apply))
      })
  }

  val min = Directive { case (FocalMin(_, neighborhood, target), childResults) =>
    childResults
      .map({ _.as[LazyMultibandRaster] })
      .toList.sequence
      .map({ lr =>
        ImageResult(lr.head.focal(NeighborhoodConversion(neighborhood), None, target, focal.Min.apply _))
      })
  }

  val mean = Directive { case (FocalMean(_, neighborhood, target), childResults) =>
    childResults
      .map({ _.as[LazyMultibandRaster] })
      .toList.sequence
      .map({ lr =>
        ImageResult(lr.head.focal(NeighborhoodConversion(neighborhood), None, target, focal.Mean.apply _))
      })
  }

  val median = Directive { case (FocalMedian(_, neighborhood, target), childResults) =>
    childResults
      .map({ _.as[LazyMultibandRaster] })
      .toList.sequence
      .map({ lr =>
        ImageResult(lr.head.focal(NeighborhoodConversion(neighborhood), None, target, focal.Median.apply _))
      })
  }

  val mode = Directive { case (FocalMode(_, neighborhood, target), childResults) =>
    childResults
      .map({ _.as[LazyMultibandRaster] })
      .toList.sequence
      .map({ lr =>
        ImageResult(lr.head.focal(NeighborhoodConversion(neighborhood), None, target, focal.Mode.apply _))
      })
  }

  val sum = Directive { case (FocalSum(_, neighborhood, target), childResults) =>
    childResults
      .map({ _.as[LazyMultibandRaster] })
      .toList.sequence
      .map({ lr =>
        ImageResult(lr.head.focal(NeighborhoodConversion(neighborhood), None, target, focal.Sum.apply _))
      })
  }

  val standardDeviation = Directive { case (FocalStdDev(_, neighborhood, target), childResults) =>
    childResults
      .map({ _.as[LazyMultibandRaster] })
      .toList.sequence
      .map({ lr =>
        ImageResult(lr.head.focal(NeighborhoodConversion(neighborhood), None, target, focal.StandardDeviation.apply _))
      })
  }

  val slope = Directive { case (FocalSlope(_, zf, target), childResults) =>
    childResults
      .map({ _.as[LazyMultibandRaster] })
      .toList.sequence
      .map({ lr =>
        val image = lr.head
        val re = image.rasterExtent
        val zfactor = zf.getOrElse {
          val llExtent = re.extent.reproject(image.crs, LatLng)
          val middleY = llExtent.ymax - (llExtent.ymax - llExtent.ymin)
          val EQUATOR_METERS = 11320
          1 / (EQUATOR_METERS * math.cos(math.toRadians(middleY)))
        }
        ImageResult(image.slope(None, zfactor, re.cellSize, target))
      })
  }

  val hillshade = Directive { case (FocalHillshade(_, azimuth, altitude, zf, target), childResults) =>
    childResults
      .map({ _.as[LazyMultibandRaster] })
      .toList.sequence
      .map({ lr =>
        val image = lr.head
        val re = image.rasterExtent
        val zfactor = zf.getOrElse {
          val llExtent = re.extent.reproject(image.crs, LatLng)
          val middleY = llExtent.ymax - (llExtent.ymax - llExtent.ymin)
          val EQUATOR_METERS = 11320
          1 / (EQUATOR_METERS * math.cos(math.toRadians(middleY)))
        }
        ImageResult(image.hillshade(None, zfactor, re.cellSize, azimuth, altitude, target))
      })
  }

  val aspect = Directive { case (FocalAspect(_, target), childResults) =>
    childResults
      .map({ _.as[LazyMultibandRaster] })
      .toList.sequence
      .map({ lr =>
        val image = lr.head
        val re = image.rasterExtent
        ImageResult(image.aspect(None, re.cellSize, target))
      })
  }
}
