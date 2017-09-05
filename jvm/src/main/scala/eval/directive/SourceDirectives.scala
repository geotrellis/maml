package maml.eval.directive

import maml.eval._
import maml.eval.tile._
import maml.ast._
import maml.ast.jvm._

import cats.data.{NonEmptyList => NEL, _}
import Validated._


object SourceDirectives {
  def intLiteralDirective[T] = Directive[T] { case (IntLiteral(int, _), _) => Valid(IntResult(int)) }

  def dblLiteralDirective[T] = Directive[T] { case (DoubleLiteral(dbl, _), _) => Valid(DoubleResult(dbl)) }

  def boolLiteralDirective[T] = Directive[T] { case (BoolLiteral(bool, _), _) => Valid(BoolResult(bool)) }

  def tileLiteralDirective[T] = Directive[T] { case (TileLiteral(tile, _), _) => Valid(TileResult(LazyTile(tile))) }

  def bufferingTileLiteralDirective[T] = ScopedDirective[BufferingInterpreter.Scope, T] { case (TileLiteral(tile, _), _, scope) =>
    Valid(TileResult(LazyTile(tile)))
  }

  //val valueReaderTileSourceDirective = Directive { case (ValueReaderTileSource(bucket, root), _) =>
  //}
}
