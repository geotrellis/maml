package maml.ast

import maml.ast.metadata._

import io.circe.generic.JsonCodec

import java.util.UUID


trait Source extends MamlTree {
  val `type`: String
  def args: List[MamlTree] = List.empty

  def find(id: UUID): Option[MamlTree] =
    if (this.id == id) Some(this)
    else None
}

case class ScalarSource(id: UUID, scalar: Double, metadata: Option[NodeMetadata]) extends Source {
  val `type`: String = "scalar"
  def sources: Seq[Source] = List()
}

case class TileSource(id: UUID, metadata: Option[NodeMetadata]) extends Source {
  val `type`: String = "tile"
  def sources: Seq[Source] = List(this)
}

