package maml.ast.leaf

import maml.ast.metadata.NodeMetadata
import maml.ast.codec._

import io.circe.generic.JsonCodec

import java.util.UUID


@JsonCodec
case class ScalarSource(id: UUID, constant: Double, metadata: Option[NodeMetadata]) extends Leaf("const") {
  def sources: Seq[Leaf] = List()
}

