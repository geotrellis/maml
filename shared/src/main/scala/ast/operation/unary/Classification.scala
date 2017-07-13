package maml.ast.operation.unary

import maml.ast.MamlTree
import maml.ast.metadata.NodeMetadata
import maml.ast.utility._

import io.circe._

import java.util.UUID


case class Classification(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata], classMap: ClassMap)
    extends UnaryOperation("classify")

