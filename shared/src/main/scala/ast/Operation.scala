package maml.ast

import maml.ast.utility._
import maml.ast.metadata._

import java.util.UUID


trait Operation extends MamlTree with Serializable {
  val symbol: String

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
}

case class Subtraction(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata])
    extends Operation {
  val symbol: String = "-"
}

case class Multiplication(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata])
    extends Operation {
  val symbol = "*"
}

case class Division(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata])
    extends Operation {
  val symbol: String = "/"
}

case class Max(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata])
    extends Operation {
  val symbol: String = "max"
}

case class Min(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata])
    extends Operation {
  val symbol: String = "min"
}

case class Masking(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata], mask: MultiPolygon)
    extends Operation {
  val symbol: String = "mask"
}

