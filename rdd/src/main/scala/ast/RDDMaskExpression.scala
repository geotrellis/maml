package maml.rdd.ast

import maml.rdd.eval._
import maml.ast._
import maml.eval._

import geotrellis.vector._
import geotrellis.spark.mask._


case class RDDMask(
  children: List[Expression],
  maskingArea: Either[Traversable[Polygon], Traversable[MultiPolygon]],
  maskingOptions: Mask.Options = Mask.Options.DEFAULT
) extends UnaryExpression with Operation {
  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
  def kindDerivation: Map[MamlKind, MamlKind] = Map(MamlKind.Tile -> MamlKind.Tile)
}

object RDDMask {
  def apply(children: List[Expression], extent: Extent): RDDMask = RDDMask(children, extent.toPolygon)

  def apply(children: List[Expression], extent: Extent, options: Mask.Options): RDDMask = RDDMask(children, extent.toPolygon, options)

  def apply(children: List[Expression], polygon: Polygon): RDDMask = RDDMask(children, Left(Seq(polygon)))

  def apply(children: List[Expression], polygon: Polygon, options: Mask.Options): RDDMask = RDDMask(children, Left(Seq(polygon)), options)

  def apply(children: List[Expression], polygon: MultiPolygon): RDDMask = RDDMask(children, Right(Seq(polygon)))

  def apply(children: List[Expression], polygon: MultiPolygon, options: Mask.Options): RDDMask = RDDMask(children, Right(Seq(polygon)), options)
}
