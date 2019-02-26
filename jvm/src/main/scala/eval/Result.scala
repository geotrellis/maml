package com.azavea.maml.eval

import com.azavea.maml.ast._
import com.azavea.maml.error._
import com.azavea.maml.eval._
import com.azavea.maml.eval.tile._

import geotrellis.raster._
import geotrellis.raster.io.geotiff.MultibandGeoTiff
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
      Invalid(NEL.of(DivergingTypes(cls.getName, List("int", "double"))))
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
      Invalid(NEL.of(DivergingTypes(cls.getName, List("int", "double"))))
  }
  def kind: MamlKind = MamlKind.Int
}

case class GeomResult(res: Geometry) extends Result {
  def as[T](implicit ct: ClassTag[T]): Interpreted[T] = {
    val cls = ct.runtimeClass
    if (classOf[Geometry] isAssignableFrom cls)
      Valid(res.asInstanceOf[T])
    else
      Invalid(NEL.of(DivergingTypes(cls.getName, List("geom"))))
  }
  def kind: MamlKind = MamlKind.Geom
}

case class ImageResult(res: LazyMultibandRaster) extends Result {
  def as[T](implicit ct: ClassTag[T]): Interpreted[T] = {
    val cls = ct.runtimeClass
    if (classOf[LazyMultibandRaster] isAssignableFrom cls)
      Valid(res.asInstanceOf[T])
    else if (classOf[ProjectedRaster[MultibandTile]] isAssignableFrom cls)
      Valid(res.evaluateDouble.asInstanceOf[T])
    else if (classOf[MultibandTile] isAssignableFrom cls)
      Valid(res.evaluateDouble.tile.asInstanceOf[T])
    else
      Invalid(NEL.of(DivergingTypes(cls.getName, List("img"))))
  }
  def kind: MamlKind = MamlKind.Image
}

case class BoolResult(res: Boolean) extends Result {
  def as[T](implicit ct: ClassTag[T]): Interpreted[T] = {
    val cls = ct.runtimeClass
    if (classOf[Boolean] isAssignableFrom cls)
      Valid(res.asInstanceOf[T])
    else
      Invalid(NEL.of(DivergingTypes(cls.getName, List("bool"))))
  }
  def kind: MamlKind = MamlKind.Bool
}

