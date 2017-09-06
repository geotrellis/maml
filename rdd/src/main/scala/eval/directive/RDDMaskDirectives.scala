package maml.rdd.eval.directive

import maml.rdd.ast._
import maml.rdd.eval._

import maml.ast._
import maml.eval._
import maml.eval.directive._

import geotrellis.raster._
import geotrellis.spark._
import geotrellis.vector._

import cats._
import cats.implicits._
import cats.data.{NonEmptyList => NEL, _}
import Validated._

import org.apache.spark.rdd._

import scala.reflect.ClassTag


object RDDMaskDirectives {
  val maskDirective = Directive { case (fn@RDDMask(_, maskingArea, options), childResults) => {
    childResults
      .head
      .as[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]]
      .map { rdd =>
        maskingArea match {
          case Left(poly) => SpatialRDDResult(rdd.mask(poly, options))
          case Right(multiPoly) => SpatialRDDResult(rdd.mask(multiPoly, options))
        }
      }
  }}
}
