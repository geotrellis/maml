package com.azavea.maml.eval

import com.azavea.maml.ast._
import com.azavea.maml.eval._
import com.azavea.maml.eval.tile.TileLayouts

import geotrellis.raster._
import geotrellis.raster.render._
import geotrellis.spark._
import geotrellis.spark.io._
import geotrellis.spark.io.s3._
import geotrellis.spark.tiling._

import cats.data.{NonEmptyList => NEL}
import cats.data.Validated._
import cats.implicits._
import scala.concurrent._
import scala.util.{Try, Success, Failure}


class Resolver(ec: ExecutionContext) {
  implicit val execution = ec
  def tmsLiteral(exp: Expression): (Int, Int, Int) => Future[Interpreted[Expression]] = (z: Int, x: Int, y: Int) => {
    lazy val extent = TileLayouts(z).mapTransform(SpatialKey(x,y))
    exp match {
      case ValueReaderTileSource(bucket, root, layerId) => Future {
        val reader = S3ValueReader(bucket, root).reader[SpatialKey, Tile](LayerId(layerId, z))
        Try {
          reader.read(x, y)
        } match {
          case Success(tile) => Valid(TileLiteral(tile, RasterExtent(tile, extent)))
          case Failure(e: ValueNotFoundError) => Invalid(NEL.of(S3TileResolutionError(exp, Some((z, x, y)))))
          case Failure(e) =>  Invalid(NEL.of(UnknownTileResolutionError(exp, Some((z, x, y)))))
        }
      }
      case _ =>
        exp.children
          .map({ child => tmsLiteral(child)(z, x, y) })
          .toList.sequence
          .map({ futureValidChildren => futureValidChildren.toList.sequence })
          .map({ children =>
            children.map({ exp.withChildren(_) })
          })
    }
  }
}

