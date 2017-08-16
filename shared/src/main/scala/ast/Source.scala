package maml.ast

import io.circe.generic.JsonCodec

import java.util.UUID


trait Source extends Expression {
  val children: List[Expression] = List.empty
  override val sources: List[Source] = List(this)
}

case class IntSource(int: Int) extends Source {
  val kind = MamlKind.Int
}

case class DoubleSource(double: Double) extends Source {
  val kind = MamlKind.Double
}

case object TileSource extends Source {
  val kind = MamlKind.Tile
}

case object GeomSource extends Source {
  val kind = MamlKind.Geom
}

case object BoolSource extends Source {
  val kind = MamlKind.Bool
}
