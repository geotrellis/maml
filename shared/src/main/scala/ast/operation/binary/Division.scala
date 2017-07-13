package maml.ast.operation.binary

import maml.ast.operation.Operation
import maml.ast.metadata._
import maml.ast.utility._
import maml.ast._

import io.circe._

import java.util.UUID


case class Division(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata])
    extends Operation("/")

