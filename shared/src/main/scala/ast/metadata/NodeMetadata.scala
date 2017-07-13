package maml.ast.metadata

import maml.ast.codec._
import maml.ast.utility._

import io.circe.generic.JsonCodec


@JsonCodec
case class NodeMetadata(
  label: Option[String] = None,
  description: Option[String] = None,
  histogram: Option[Histogram] = None,
  colorRamp: Option[ColorRamp] = None,
  classMap: Option[ClassMap] = None,
  breaks: Option[Vector[Double]] = None
) {

  /** A helper method for merging default values with overrides */
  def fallbackTo(that: NodeMetadata): NodeMetadata = {
    NodeMetadata(
      this.label.orElse(that.label),
      this.description.orElse(that.description),
      this.histogram.orElse(that.histogram),
      this.colorRamp.orElse(that.colorRamp),
      this.classMap.orElse(that.classMap)
    )
  }
}

