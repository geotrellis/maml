package maml.ast

import maml.ast.metadata._

import io.circe.generic.JsonCodec

import java.util.UUID


trait Source extends MamlTree {
  val symbol: String
  def args: List[MamlTree] = List.empty

  def find(id: UUID): Option[MamlTree] =
    if (this.id == id) Some(this)
    else None
}

@JsonCodec
case class ScalarSource(id: UUID, constant: Double, metadata: Option[NodeMetadata]) extends Source {
  val symbol: String = "scalar"
  def sources: Seq[Source] = List()
}

@JsonCodec
case class TileSource(id: UUID, metadata: Option[NodeMetadata]) extends Source {
  val symbol: String = "tile"
  def sources: Seq[Source] = List(this)
}

