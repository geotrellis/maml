package maml.ast.operation.binary

import maml.ast.operation.Operation
import maml.ast.MamlTree
import maml.ast.metadata._
import maml.ast.utility._

import io.circe._

import java.util.UUID


case class Subtraction(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata])
    extends Operation("-")

