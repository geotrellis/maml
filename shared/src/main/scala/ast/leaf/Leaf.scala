package maml.ast.leaf

import maml.ast._

import java.util.UUID


abstract class Leaf(val `type`: String) extends MamlTree {
  def args: List[MamlTree] = List.empty

  def find(id: UUID): Option[MamlTree] =
    if (this.id == id) Some(this)
    else None
}

