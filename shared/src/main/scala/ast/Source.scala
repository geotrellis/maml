package maml.ast

import io.circe.generic.JsonCodec

import java.util.UUID


trait Source extends Expression {
  def id: String

  val children: List[Expression] = List.empty
  def withChildren(children: List[Expression]) = this
  override val sources: List[Source] = List(this)
}

case class IntLiteral(value: Int) extends Source {
  def id = value.toString
  val kind = MamlKind.Int
}

case class DoubleLiteral(value: Double) extends Source {
  def id = value.toString
  val kind = MamlKind.Double
}

case class BoolLiteral(value: Boolean) extends Source {
  def id = value.toString
  val kind = MamlKind.Bool
}

trait UnboundSource extends Source

case class IntSource(id: String) extends UnboundSource {
  val kind = MamlKind.Int
}

case class DoubleSource(id: String) extends UnboundSource {
  val kind = MamlKind.Double
}

case class TileSource(id: String) extends UnboundSource {
  val kind = MamlKind.Tile
}

case class GeomSource(id: String) extends UnboundSource {
  val kind = MamlKind.Geom
}

case class BoolSource(id: String) extends UnboundSource {
  val kind = MamlKind.Bool
}
