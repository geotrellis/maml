package com.azavea.maml.ast

import com.azavea.maml.eval._
import com.azavea.maml.eval.tile._
import com.azavea.maml.util._

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
import java.net.URI


/**
 * Reference implementation of UnboundTileSource which allows serving tiles with AWS S3 backing */
case class CogSource(uri: URI, band: Int, celltype: Option[CellType]) extends UnboundTileSource {
  val kind = MamlKind.Tile

  def resolveBindingForTmsTile(zoom: Int, x: Int, y: Int)(implicit t: Timer[IO]): IO[BoundSource] =
    resolveBindingForTmsTile(zoom, x, y, 0)

  def resolveBindingForTmsTile(zoom: Int, x: Int, y: Int, buffer: Int)(implicit t: Timer[IO]): IO[BoundSource] = {
    lazy val emptyTile = celltype match {
      case Some(ct) => IntConstantTile(NODATA, 256, 256).convert(ct)
      case None     => IntConstantTile(NODATA, 256, 256)
    }
    def fetch(xCoord: Int, yCoord: Int) =
      CogResolution.fetchTmsTile(uri.toString, zoom, x, y).map(_.band(band))

    (fetch(x - 1, y - 1), fetch(x, y - 1), fetch(x + 1, y - 1),
     fetch(x - 1, y),     fetch(x, y),     fetch(x + 1, y),
     fetch(x - 1, y + 1), fetch(x, y + 1), fetch(x + 1, y + 1)
    ).parMapN { (tl, tm, tr, ml, mm, mr, bl, bm, br) =>
      val extent = TileLayouts(zoom).mapTransform(SpatialKey(x, y))
      val tile = TileWithNeighbors(mm, Some(NeighboringTiles(tl, tm, tr, ml, mr, bl, bm, br))).withBuffer(buffer)
      TileLiteral(tile, RasterExtent(tile, extent))
    }
  }

  def resolveBindingForExtent(zoom: Int, extent: Extent)(implicit t: Timer[IO]): IO[BoundSource] =
    CogResolution.fetchExtent(uri.toString, zoom, extent).map { mbt =>
      val tile = mbt.band(band)
      TileLiteral(tile, RasterExtent(tile, extent))
    }
}


object CogSource {
  implicit def conf: Configuration =
    Configuration.default.withDefaults.withDiscriminator("type")

  implicit val cellTypeEncoder: Encoder[CellType] =
    Encoder.encodeString.contramap[CellType] { _.toString }
  implicit val cellTypeDecoder: Decoder[CellType] =
    Decoder.decodeString.emap { str =>
      Either.catchNonFatal(CellType.fromName(str)).leftMap(_ => "CellType")
    }

  implicit val uriEncoder: Encoder[URI] =
    Encoder.encodeString.contramap[URI] { _.toString }
  implicit val ruDecoder: Decoder[URI] =
    Decoder.decodeString.emap { str =>
      Either.catchNonFatal(new URI(str)).leftMap(_ => "URI")
    }

  implicit lazy val decodeCogTileSource: Decoder[CogSource] = deriveDecoder
  implicit lazy val encodeCogTileSource: Encoder[CogSource] = deriveEncoder
}

