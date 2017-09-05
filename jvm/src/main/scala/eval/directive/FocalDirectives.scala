package maml.eval.directive

import maml.eval._
import maml.ast._
import maml.eval.tile._
import maml.ast.utility._

import geotrellis.raster.Tile
import geotrellis.raster.mapalgebra.focal
import cats._
import cats.implicits._
import cats.data.{NonEmptyList => NEL, _}
import Validated._


object FocalDirectives {
  def focalMaxDirective[T] = Directive[T] { case (fm@FocalMax(_, neighborhood, _), childResults) =>
    childResults
      .map({ _.as[LazyTile] })
      .toList.sequence
      .map({ lt =>
        TileResult(LazyTile.Focal(lt, neighborhood, None, focal.Max.apply _))
      })
  }

  def focalMinDirective[T] = Directive[T] { case (fm@FocalMin(_, neighborhood, _), childResults) =>
    childResults
      .map({ _.as[LazyTile] })
      .toList.sequence
      .map({ lt =>
        TileResult(LazyTile.Focal(lt, neighborhood, None, focal.Min.apply _))
      })
  }

  def focalMeanDirective[T] = Directive[T] { case (fm@FocalMean(_, neighborhood, _), childResults) =>
    childResults
      .map({ _.as[LazyTile] })
      .toList.sequence
      .map({ lt =>
        TileResult(LazyTile.Focal(lt, neighborhood, None, focal.Mean.apply _))
      })
  }

  def focalMedianDirective[T] = Directive[T] { case (fm@FocalMedian(_, neighborhood, _), childResults) =>
    childResults
      .map({ _.as[LazyTile] })
      .toList.sequence
      .map({ lt =>
        TileResult(LazyTile.Focal(lt, neighborhood, None, focal.Median.apply _))
      })
  }

  def focalModeDirective[T] = Directive[T] { case (fm@FocalMode(_, neighborhood, _), childResults) =>
    childResults
      .map({ _.as[LazyTile] })
      .toList.sequence
      .map({ lt =>
        TileResult(LazyTile.Focal(lt, neighborhood, None, focal.Mode.apply _))
      })
  }

  def focalSumDirective[T] = Directive[T] { case (fm@FocalSum(_, neighborhood, _), childResults) =>
    childResults
      .map({ _.as[LazyTile] })
      .toList.sequence
      .map({ lt =>
        TileResult(LazyTile.Focal(lt, neighborhood, None, focal.Sum.apply _))
      })
  }

  def focalStandardDeviationDirective[T] = Directive[T] { case (fm@FocalStdDev(_, neighborhood, _), childResults) =>
    childResults
      .map({ _.as[LazyTile] })
      .toList.sequence
      .map({ lt =>
        TileResult(LazyTile.Focal(lt, neighborhood, None, focal.StandardDeviation.apply _))
      })
  }
}
