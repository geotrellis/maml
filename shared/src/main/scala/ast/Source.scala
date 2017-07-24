package maml.ast

import MamlKind._

import io.circe.generic.JsonCodec

import java.util.UUID


trait Source extends Expression {
  val `type`: String
  def kind: MamlKind
  def children: List[Expression] = List.empty
  override val sources: Seq[Source] = List(this)
}

case class ScalarSource(scalar: Double) extends Source {
  val `type`: String = "scalar"
  def kind = MamlScalar
}

case object TileSource extends Source {
  val `type`: String = "tile"
  def kind = MamlTile
}

case object VectorSource extends Source {
  val `type`: String = "tile"
  def kind = MamlVector
}

