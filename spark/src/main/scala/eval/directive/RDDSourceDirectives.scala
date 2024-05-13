package com.azavea.maml.spark.eval.directive

import com.azavea.maml.eval.directive._
import com.azavea.maml.eval._
import com.azavea.maml.error._
import com.azavea.maml.ast._
import com.azavea.maml.spark.eval._

import geotrellis.spark._
import geotrellis.layer._
import cats.data.{NonEmptyList => NEL, _}
import cats.data.Validated._

import scala.util.Try

object RDDSourceDirectives {
  val rddLiteral = Directive { case (rl @ RasterLit(rdd), _) =>
    Try(rdd.asInstanceOf[TileLayerRDD[SpatialKey]]).toOption match {
      case Some(rasterRdd) => Valid(RDDResult(rasterRdd))
      case None =>
        Invalid(NEL.of(NonEvaluableNode(rl, Some("Unable to treat raster literal contents as type TileLayerRDD[SpatialKey]]"))))
    }
  }
}
