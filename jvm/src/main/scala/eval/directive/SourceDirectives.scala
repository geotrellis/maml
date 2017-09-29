package com.azavea.maml.eval.directive

import com.azavea.maml.eval._
import com.azavea.maml.eval.tile._
import com.azavea.maml.ast._

import cats.data.{NonEmptyList => NEL, _}
import Validated._


object SourceDirectives {
  val intLiteral = Directive { case (IntLiteral(int), _) => Valid(IntResult(int)) }

  val dblLiteral = Directive { case (DoubleLiteral(dbl), _) => Valid(DoubleResult(dbl)) }

  val boolLiteral = Directive { case (BoolLiteral(bool), _) => Valid(BoolResult(bool)) }

  val tileLiteral = Directive { case (TileLiteral(tile), _) => Valid(TileResult(LazyTile(tile))) }

  val bufferingTileLiteral = ScopedDirective[BufferingInterpreter.Scope] { case (TileLiteral(tile), _, scope) =>
    Valid(TileResult(LazyTile(tile)))
  }
}
