package maml.ast

import io.circe.generic.JsonCodec
import geotrellis.raster.Tile

import java.util.UUID


trait Source extends Expression {
  val children: List[Expression] = List.empty
  override val sources: List[Source] = List(this)
}

case class IntLiteral(value: Int) extends Source {
  val kind = MamlKind.Int
}

case class DoubleLiteral(value: Double) extends Source {
  val kind = MamlKind.Double
}

case class BoolLiteral(value: Boolean) extends Source {
  val kind = MamlKind.Bool
}

case class TileLiteral(id: Tile) extends Source {
  val kind = MamlKind.Tile
}

trait Variable extends Source {
  def id: String
}

case class IntSource(id: String) extends Variable {
  val kind = MamlKind.Int
}

case class DoubleSource(id: String) extends Variable {
  val kind = MamlKind.Double
}

case class TileSource(id: String) extends Variable {
  val kind = MamlKind.Tile
}

case class GeomSource(id: String) extends Variable {
  val kind = MamlKind.Geom
}

case class BoolSource(id: String) extends Variable {
  val kind = MamlKind.Bool
}
