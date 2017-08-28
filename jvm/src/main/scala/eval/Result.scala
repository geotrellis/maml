package maml.eval

import maml.error._
import maml.ast._
import maml.eval._
import maml.eval.tile._

import geotrellis.raster.Tile
import geotrellis.vector.Geometry
import cats.data.{NonEmptyList => NEL, _}
import Validated._

import scala.reflect.ClassTag


trait Result {
  def as[T](implicit ct: ClassTag[T]): Interpreted[T]
  def kind: MamlKind
}

case class DoubleResult(res: Double) extends Result {
  def as[T](implicit ct: ClassTag[T]): Interpreted[T] = {
    val cls = ct.runtimeClass
    if (classOf[Int] isAssignableFrom cls)
      Valid(res.toInt.asInstanceOf[T])
    else if (classOf[Double] isAssignableFrom cls)
      Valid(res.asInstanceOf[T])
    else
      Invalid(NEL.of(EvalTypeError(cls.getName, List("int", "double"))))
  }
  def kind: MamlKind = MamlKind.Double
}

case class IntResult(res: Int) extends Result {
  def as[T](implicit ct: ClassTag[T]): Interpreted[T] = {
    val cls = ct.runtimeClass
    if (classOf[Int] isAssignableFrom cls)
      Valid(res.toInt.asInstanceOf[T])
    else if (classOf[Double] isAssignableFrom cls)
      Valid(res.toDouble.asInstanceOf[T])
    else
      Invalid(NEL.of(EvalTypeError(cls.getName, List("int", "double"))))
  }
  def kind: MamlKind = MamlKind.Int
}

case class GeomResult(res: Geometry) extends Result {
  def as[T](implicit ct: ClassTag[T]): Interpreted[T] = {
    val cls = ct.runtimeClass
    if (classOf[Geometry] isAssignableFrom cls)
      Valid(res.asInstanceOf[T])
    else
      Invalid(NEL.of(EvalTypeError(cls.getName, List("geom"))))
  }
  def kind: MamlKind = MamlKind.Geom
}

case class TileResult(res: LazyTile) extends Result {
  def as[T](implicit ct: ClassTag[T]): Interpreted[T] = {
    val cls = ct.runtimeClass
    if (classOf[Tile] isAssignableFrom cls)
      Valid(res.evaluate.asInstanceOf[T])
    else if (classOf[LazyTile] isAssignableFrom cls)
      Valid(res.asInstanceOf[T])
    else
      Invalid(NEL.of(EvalTypeError(cls.getName, List("Tile"))))
  }
  def kind: MamlKind = MamlKind.Tile
}

case class BoolResult(res: Boolean) extends Result {
  def as[T](implicit ct: ClassTag[T]): Interpreted[T] = {
    val cls = ct.runtimeClass
    if (classOf[Boolean] isAssignableFrom cls)
      Valid(res.asInstanceOf[T])
    else
      Invalid(NEL.of(EvalTypeError(cls.getName, List("bool"))))
  }
  def kind: MamlKind = MamlKind.Bool
}

