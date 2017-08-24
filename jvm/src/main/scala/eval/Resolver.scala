package maml.eval

import maml.ast._
import maml.ast.jvm.TileLiteral
import maml.error._

import geotrellis.raster._
import geotrellis.raster.render._
import geotrellis.spark._
import geotrellis.spark.io._
import geotrellis.spark.io.s3._

import cats.data.{NonEmptyList => NEL}
import cats.data.Validated._
import cats.implicits._
import scala.concurrent._
import scala.util.{Try, Success, Failure}


object Resolver {
  def tmsLiteral(exp: Expression)(implicit ec: ExecutionContext): (Int, Int, Int) => Future[Interpreted[Expression]] = (z: Int, x: Int, y: Int) => {
    exp match {
      case ValueReaderTileSource(bucket, root, layerId) => Future {
        val reader = S3ValueReader(bucket, root).reader[SpatialKey, Tile](LayerId(layerId, z))
        Try {
          reader.read(x, y)
        } match {
          case Success(tile) => Valid(TileLiteral(tile))
          case Failure(e: ValueNotFoundError) => Invalid(NEL.of(S3TileResolutionError(exp, Some((z, x, y)))))
          case Failure(e) =>  Invalid(NEL.of(UnknownTileResolutionError(exp, Some((z, x, y)))))
        }
      }
      case _ =>
        exp.children
          .map({ child => tmsLiteral(child)(ec)(z, x, y) })
          .toList.sequence
          .map({ futureValidChildren => futureValidChildren.toList.sequence })
          .map({ children =>
            children.map({ exp.withChildren(_) })
          })
    }
  }
}
