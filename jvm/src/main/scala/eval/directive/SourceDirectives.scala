package com.azavea.maml.eval.directive

import com.azavea.maml.error._
import com.azavea.maml.eval._
import com.azavea.maml.eval.tile._
import com.azavea.maml.ast._

import geotrellis.vector.io._
import geotrellis.vector.Geometry
import geotrellis.raster.{Raster, Tile}
import io.circe._
import io.circe.parser._
import cats.data.{NonEmptyList => NEL, _}
import Validated._

import scala.util.{Try, Success, Failure}

object SourceDirectives {
  val intLiteral = Directive { case (IntLit(int), _) => Valid(IntResult(int)) }

  val dblLiteral = Directive { case (DblLit(dbl), _) => Valid(DoubleResult(dbl)) }

  val boolLiteral = Directive { case (BoolLit(bool), _) => Valid(BoolResult(bool)) }

  val rasterLiteral = Directive {
    case (RasterLit(r), _) if r.isInstanceOf[Raster[_]] =>
      val raster = r.asInstanceOf[Raster[Tile]]
      Valid(TileResult(LazyTile(raster.tile, raster.extent)))
    case (rl@RasterLit(r), _) =>
      Invalid(NEL.of(NonEvaluableNode(rl, Some("Unable to treat raster literal contents as type Raster"))))
  }

  val geoJson = Directive { case (GeomLit(jsonString), _) =>
    Try(jsonString.parseGeoJson[Geometry]) match {
      case Success(geom) => Valid(GeomResult(geom))
      case Failure(e) =>
        parse(jsonString) match {
          case Right(json) => Invalid(NEL.of(ASTDecodeError(json, "provided JSON is not valid GeoJson")))
          case Left(parsingFailure) => Invalid(NEL.of(ASTParseError(jsonString, parsingFailure.message)))
        }
    }
  }
}

