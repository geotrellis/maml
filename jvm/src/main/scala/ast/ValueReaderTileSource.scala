package com.azavea.maml.ast

import com.azavea.maml.eval._
import com.azavea.maml.eval.tile._

import io.circe._
import io.circe.syntax._
import io.circe.generic.extras.semiauto._
import io.circe.generic.extras.Configuration
import cats._
import cats.effect._
import cats.syntax.all._
import geotrellis.vector.Extent
import geotrellis.raster.{IntConstantTile, MultibandTile, NODATA, CellType, Tile, RasterExtent}
import geotrellis.spark._
import geotrellis.spark.io._
import geotrellis.spark.io.s3.{S3ValueReader, S3CollectionLayerReader}


import java.security.InvalidParameterException


case class S3ValueReaderTileSource(bucket: String, root: String, layerId: String, band: Int, celltype: Option[CellType]) extends UnboundTileSource {
  val kind = MamlKind.Tile

  def resolveBindingForTmsTile(z: Int, x: Int, y: Int)(implicit t: Timer[IO]): IO[BoundSource] =
    resolveBindingForTmsTile(z, x, y, 0)

  def resolveBindingForTmsTile(z: Int, x: Int, y: Int, buffer: Int)(implicit t: Timer[IO]): IO[BoundSource] = {
    lazy val emptyTile = celltype match {
      case Some(ct) => IntConstantTile(NODATA, 256, 256).convert(ct)
      case None     => IntConstantTile(NODATA, 256, 256)
    }
    val reader = S3ValueReader(bucket, root).reader[SpatialKey, MultibandTile](LayerId(layerId, z))
    val recover = PartialFunction[Throwable, IO[Tile]] { case lre: LayerReadError => IO.pure(emptyTile) }
    def fetch(xCoord: Int, yCoord: Int) = IO(reader.read(SpatialKey(xCoord, yCoord)))
    (fetch(x - 1, y - 1).map(_.band(band)).recoverWith(recover),
     fetch(x,     y - 1).map(_.band(band)).recoverWith(recover),
     fetch(x + 1, y - 1).map(_.band(band)).recoverWith(recover),
     fetch(x - 1,     y).map(_.band(band)).recoverWith(recover),
     fetch(x,         y).map(_.band(band)).recoverWith(recover),
     fetch(x + 1,     y).map(_.band(band)).recoverWith(recover),
     fetch(x - 1, y + 1).map(_.band(band)).recoverWith(recover),
     fetch(x,     y + 1).map(_.band(band)).recoverWith(recover),
     fetch(x + 1, y + 1).map(_.band(band)).recoverWith(recover)
    ).parMapN { (tl, tm, tr, ml, mm, mr, bl, bm, br) =>
      val extent = TileLayouts(z).mapTransform(SpatialKey(x, y))
      val tile = TileWithNeighbors(mm, Some(NeighboringTiles(tl, tm, tr, ml, mr, bl, bm, br))).withBuffer(buffer)
      TileLiteral(tile, RasterExtent(tile, extent))
    }
  }

  def resolveBindingForExtent(extent: Extent, zoom: Int)(implicit t: Timer[IO]): IO[BoundSource] =
    resolveBindingForExtent(extent, zoom, 0)

  def resolveBindingForExtent(extent: Extent, zoom: Int, buffer: Int)(implicit t: Timer[IO]): IO[BoundSource] =
    IO {
      val reader = S3CollectionLayerReader(bucket, root)
      val query = new LayerQuery[SpatialKey, TileLayerMetadata[SpatialKey]].where(Intersects(extent.buffer(buffer.toDouble)))
      reader.read[SpatialKey, MultibandTile, TileLayerMetadata[SpatialKey]](LayerId(layerId, zoom), query)
    } map { tiles =>
      tiles.map(_._2.band(band)).reduce(_ merge _)
    } map { merged =>
      TileLiteral(merged, RasterExtent(merged, extent))
    }
}


object S3ValueReaderTileSource {
  implicit def conf: Configuration =
    Configuration.default.withDefaults.withDiscriminator("type")

  implicit val cellTypeEncoder: Encoder[CellType] =
    Encoder.encodeString.contramap[CellType] { _.toString }
  implicit val cellTypeDecoder: Decoder[CellType] =
    Decoder.decodeString.emap { str =>
      Either.catchNonFatal(CellType.fromName(str)).leftMap(_ => "CellType")
    }

  implicit lazy val decodeValueReaderTileSource: Decoder[S3ValueReaderTileSource] = deriveDecoder
  implicit lazy val encodeValueReaderTileSource: Encoder[S3ValueReaderTileSource] = deriveEncoder
}

