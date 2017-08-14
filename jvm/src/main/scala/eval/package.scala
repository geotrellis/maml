package maml

import maml.ast._
import maml.ast.TileSource
import maml.error._
import maml.eval.tile._
import maml.eval.scalar._

import geotrellis.raster._
import cats._
import cats.data._
import cats.data.Validated._
import cats.implicits._


package object eval {
  type Interpreted[A] = ValidatedNel[InterpreterError, A]

  //implicit class InterpretableExpression(val self: Expression) extends Interpretable[Expression] {
  //  def interpret = self match {
  //    case exp: ScalarSource => exp.interpret
  //    case exp: TileSource => exp.interpret
  //    case exp: Addition => exp.interpret
  //  }
  //}

  //implicit class InterpretableScalar(val self: ScalarSource) extends Interpretable[ScalarSource] {
  //  def interpret = Scalar(self.scalar)
  //}

  //implicit class InterpretableTile(val self: TileSource) extends Interpretable[TileSource] {
  //  //def interpret = ???
  //  def interpret = LazyTile(IntArrayTile(1 to 16 toArray, 4, 4))
  //}

  //implicit class InterpretableAddition(val self: Addition) extends Interpretable[Addition] {
  //  def interpret = {
  //    if (self.kind == MamlKind.Scalar) {
  //      val children = self.children.map(_.interpret).asInstanceOf[Array[ScalarRep]]
  //      children.reduce(ScalarDualCombine(_, _, { _ + _ }, { _ + _ }))

  //    } else if (self.kind == MamlKind.Tile) {
  //      val grouped = self.children.groupBy(_.kind)

  //      val scalar =
  //        grouped(MamlKind.Scalar)
  //          .map(_.interpret)
  //          .reduce(ScalarDualCombine(_, _, { _ + _ }, { _ + _ }))
  //          .asInstanceOf[ScalarRep]

  //      val tile =
  //        grouped(MamlKind.Tile)
  //          .map(_.interpret)
  //          .reduce({ (t1: LazyTile, t2: LazyTile) =>
  //            LazyTile.DualCombine(Array(t1, t2), { _ + _ }, { _ + _ })
  //          })

  //      if (scalar.isEmpty)
  //        tile
  //      else
  //        LazyTile.ScalarTileDualCombine(Array(tile), scalar, { _ + _ }, { _ + _ })
  //    } else ???
  //  }
  //}
}

