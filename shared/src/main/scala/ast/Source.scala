package maml.ast

import io.circe.generic.JsonCodec

import java.util.UUID


trait Source extends Expression {
  def kind: MamlKind
  def children: List[Expression] = List.empty
  override val sources: Seq[Source] = List(this)
}

case class ScalarSource(scalar: Double) extends Source {
  def kind = MamlKind.Scalar
}

case object TileSource extends Source {
  def kind = MamlKind.Tile
}

case object VectorSource extends Source {
  def kind = MamlKind.Vector
}

