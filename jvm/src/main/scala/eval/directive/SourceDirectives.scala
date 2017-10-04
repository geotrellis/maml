package com.azavea.maml.eval.directive

import com.azavea.maml.eval._
import com.azavea.maml.eval.tile._
import com.azavea.maml.ast._

import geotrellis.vector.io._
import geotrellis.vector.Geometry
import io.circe._
import io.circe.parser._
import cats.data.{NonEmptyList => NEL, _}
import Validated._

import scala.util.{Try, Success, Failure}

object SourceDirectives {
  val intLiteral = Directive { case (IntLiteral(int), _) => Valid(IntResult(int)) }

  val dblLiteral = Directive { case (DoubleLiteral(dbl), _) => Valid(DoubleResult(dbl)) }

  val boolLiteral = Directive { case (BoolLiteral(bool), _) => Valid(BoolResult(bool)) }

  val tileLiteral = Directive { case (TileLiteral(tile, extent), _) => Valid(TileResult(LazyTile(tile, extent))) }

  val geomJson = Directive { case (GeomJson(jsonString), _) =>
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

