package maml.ast

import maml.ast.utility._
import maml.ast.metadata._

import java.util.UUID


trait Operation extends MamlTree with Serializable {
  val symbol: String
  def withArgs(newArgs: List[MamlTree]): MamlTree

  def find(id: UUID): Option[MamlTree] =
    if (this.id == id)
      Some(this)
    else {
      val matches = args.flatMap(_.find(id))
      matches.headOption
    }

  def sources: Seq[Source] = args.flatMap(_.sources).distinct
}


case class Addition(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata])
    extends Operation {
  val symbol: String = "+"
  def withArgs(newArgs: List[MamlTree]): MamlTree = copy(args = newArgs)
}

case class Subtraction(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata])
    extends Operation {
  val symbol: String = "-"
  def withArgs(newArgs: List[MamlTree]): MamlTree = copy(args = newArgs)
}

case class Multiplication(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata])
    extends Operation {
  val symbol = "*"
  def withArgs(newArgs: List[MamlTree]): MamlTree = copy(args = newArgs)
}

case class Division(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata])
    extends Operation {
  val symbol: String = "/"
  def withArgs(newArgs: List[MamlTree]): MamlTree = copy(args = newArgs)
}

case class Max(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata])
    extends Operation {
  val symbol: String = "max"
  def withArgs(newArgs: List[MamlTree]): MamlTree = copy(args = newArgs)
}

case class Min(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata])
    extends Operation {
  val symbol: String = "min"
  def withArgs(newArgs: List[MamlTree]): MamlTree = copy(args = newArgs)
}


