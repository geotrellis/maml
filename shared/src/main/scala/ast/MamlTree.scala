package maml.ast

import maml.ast.metadata._

import io.circe.generic.JsonCodec

import java.util.UUID

/** The ur-type for a recursive representation of MapAlgebra operations */
trait MamlTree extends Product with Serializable {
  def id: UUID
  def args: List[MamlTree]
  def metadata: Option[NodeMetadata]
  def find(id: UUID): Option[MamlTree]
  def sources: Seq[Source]
}

