package com.azavea.maml.eval.directive

import com.azavea.maml.eval._
import com.azavea.maml.eval.tile._
import com.azavea.maml.ast._
import com.azavea.maml.ast.jvm._

import cats.data.{NonEmptyList => NEL, _}
import Validated._


object SourceDirectives {
  val intLiteralDirective = Directive { case (IntLiteral(int), _) => Valid(IntResult(int)) }

  val dblLiteralDirective = Directive { case (DoubleLiteral(dbl), _) => Valid(DoubleResult(dbl)) }

  val boolLiteralDirective = Directive { case (BoolLiteral(bool), _) => Valid(BoolResult(bool)) }

  val tileLiteralDirective = Directive { case (TileLiteral(tile), _) => Valid(TileResult(LazyTile(tile))) }
  val bufferingTileLiteralDirective = ScopedDirective[BufferingInterpreter.Scope] { case (TileLiteral(tile), _, scope) =>
    Valid(TileResult(LazyTile(tile)))
  }

  //val valueReaderTileSourceDirective = Directive { case (ValueReaderTileSource(bucket, root), _) =>
  //}
}
