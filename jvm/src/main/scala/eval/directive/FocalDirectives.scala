package com.azavea.maml.eval.directive

import com.azavea.maml.eval._
import com.azavea.maml.ast._
import com.azavea.maml.eval.tile._
import com.azavea.maml.util._

import geotrellis.raster.Tile
import geotrellis.raster.mapalgebra.focal
import cats._
import cats.implicits._
import cats.data.{NonEmptyList => NEL, _}
import Validated._


object FocalDirectives {
  val focalMax = Directive { case (fm@FocalMax(_, neighborhood), childResults) =>
    childResults
      .map({ _.as[LazyTile] })
      .toList.sequence
      .map({ lt =>
        TileResult(LazyTile.Focal(lt, NeighborhoodConversion(neighborhood), None, focal.Max.apply _))
      })
  }

  val focalMin = Directive { case (fm@FocalMin(_, neighborhood), childResults) =>
    childResults
      .map({ _.as[LazyTile] })
      .toList.sequence
      .map({ lt =>
        TileResult(LazyTile.Focal(lt, NeighborhoodConversion(neighborhood), None, focal.Min.apply _))
      })
  }

  val focalMean = Directive { case (fm@FocalMean(_, neighborhood), childResults) =>
    childResults
      .map({ _.as[LazyTile] })
      .toList.sequence
      .map({ lt =>
        TileResult(LazyTile.Focal(lt, NeighborhoodConversion(neighborhood), None, focal.Mean.apply _))
      })
  }

  val focalMedian = Directive { case (fm@FocalMedian(_, neighborhood), childResults) =>
    childResults
      .map({ _.as[LazyTile] })
      .toList.sequence
      .map({ lt =>
        TileResult(LazyTile.Focal(lt, NeighborhoodConversion(neighborhood), None, focal.Median.apply _))
      })
  }

  val focalMode = Directive { case (fm@FocalMode(_, neighborhood), childResults) =>
    childResults
      .map({ _.as[LazyTile] })
      .toList.sequence
      .map({ lt =>
        TileResult(LazyTile.Focal(lt, NeighborhoodConversion(neighborhood), None, focal.Mode.apply _))
      })
  }

  val focalSum = Directive { case (fm@FocalSum(_, neighborhood), childResults) =>
    childResults
      .map({ _.as[LazyTile] })
      .toList.sequence
      .map({ lt =>
        TileResult(LazyTile.Focal(lt, NeighborhoodConversion(neighborhood), None, focal.Sum.apply _))
      })
  }

  val focalStandardDeviation = Directive { case (fm@FocalStdDev(_, neighborhood), childResults) =>
    childResults
      .map({ _.as[LazyTile] })
      .toList.sequence
      .map({ lt =>
        TileResult(LazyTile.Focal(lt, NeighborhoodConversion(neighborhood), None, focal.StandardDeviation.apply _))
      })
  }
}
