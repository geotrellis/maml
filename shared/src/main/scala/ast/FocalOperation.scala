package maml.ast

import maml.ast._
import maml.ast.utility._
import maml.ast.metadata._

import java.util.UUID

trait FocalOperation extends UnaryOperation

case class FocalMax(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata], neighborhood: Neighborhood) extends FocalOperation {
  val symbol = "focalMax"
  def withArgs(newArgs: List[MamlTree]): MamlTree = copy(args = newArgs)
}

case class FocalMean(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata], neighborhood: Neighborhood) extends FocalOperation {
  val symbol = "focalMean"
  def withArgs(newArgs: List[MamlTree]): MamlTree = copy(args = newArgs)
}

case class FocalMedian(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata], neighborhood: Neighborhood) extends FocalOperation {
  val symbol = "focalMedian"
  def withArgs(newArgs: List[MamlTree]): MamlTree = copy(args = newArgs)
}

case class FocalMin(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata], neighborhood: Neighborhood) extends FocalOperation {
  val symbol = "focalMin"
  def withArgs(newArgs: List[MamlTree]): MamlTree = copy(args = newArgs)
}

case class FocalMode(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata], neighborhood: Neighborhood) extends FocalOperation {
  val symbol = "focalMode"
  def withArgs(newArgs: List[MamlTree]): MamlTree = copy(args = newArgs)
}

case class FocalSum(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata], neighborhood: Neighborhood) extends FocalOperation {
  val symbol = "focalSum"
  def withArgs(newArgs: List[MamlTree]): MamlTree = copy(args = newArgs)
}

case class FocalStdDev(args: List[MamlTree], id: UUID, metadata: Option[NodeMetadata], neighborhood: Neighborhood) extends FocalOperation {
  val symbol = "focalStdDev"
  def withArgs(newArgs: List[MamlTree]): MamlTree = copy(args = newArgs)
}

