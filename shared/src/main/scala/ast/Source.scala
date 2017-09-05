package maml.ast

import io.circe.generic.JsonCodec

import java.util.UUID


trait Source[T] extends Expression[T] {
  def id: String

  val children: List[Expression[T]] = List.empty
  def withChildren(children: List[Expression[T]]) = this
  override val sources: List[Source[T]] = List(this)
}

case class IntLiteral[T](value: Int, extra: T) extends Source[T] {
  def id = value.toString
  val kind = MamlKind.Int
}

case class DoubleLiteral[T](value: Double, extra: T) extends Source[T] {
  def id = value.toString
  val kind = MamlKind.Double
}

case class BoolLiteral[T](value: Boolean, extra: T) extends Source[T] {
  def id = value.toString
  val kind = MamlKind.Bool
}

trait UnboundSource[T] extends Source[T]

case class IntSource[T](id: String, extra: T) extends UnboundSource[T] {
  val kind = MamlKind.Int
}

case class DoubleSource[T](id: String, extra: T) extends UnboundSource[T] {
  val kind = MamlKind.Double
}

case class TileSource[T](id: String, extra: T) extends UnboundSource[T] {
  val kind = MamlKind.Tile
}

case class GeomSource[T](id: String, extra: T) extends UnboundSource[T] {
  val kind = MamlKind.Geom
}

case class BoolSource[T](id: String, extra: T) extends UnboundSource[T] {
  val kind = MamlKind.Bool
}
