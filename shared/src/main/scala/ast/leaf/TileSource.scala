package maml.ast.leaf

import maml.ast.codec._
import maml.ast.metadata._

import io.circe.generic.JsonCodec

import java.util.UUID


/** Map Algebra sources */
@JsonCodec
case class TileSource(id: UUID, metadata: Option[NodeMetadata]) extends Leaf("src") {
  def sources: Seq[Leaf] = List(this)
}

