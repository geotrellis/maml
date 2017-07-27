package maml.ast

import io.circe.generic.JsonCodec

import java.util.UUID


trait Source extends Expression {
  val children: List[Expression] = List.empty
  override val sources: List[Source] = List(this)
}

case class ScalarSource(scalar: Double) extends Source {
  val kind = MamlKind.Scalar
}

case object TileSource extends Source {
  val kind = MamlKind.Tile
}

case object VectorSource extends Source {
  val kind = MamlKind.Vector
}

