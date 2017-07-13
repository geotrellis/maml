package maml.ast.operation

import maml.ast.MamlTree
import maml.ast.leaf.Leaf

import java.util.UUID


abstract class Operation(val symbol: String) extends MamlTree with Serializable {
  def find(id: UUID): Option[MamlTree] =
    if (this.id == id)
      Some(this)
    else {
      val matches = args.flatMap(_.find(id))
      matches.headOption
    }

  def sources: Seq[Leaf] = args.flatMap(_.sources).distinct
}

