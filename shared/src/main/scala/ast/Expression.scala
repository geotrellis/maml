package maml.ast

import io.circe.generic.JsonCodec

import java.util.UUID

case class ExpressionPath(indices: Seq[Int])

/** The ur-type for a recursive representation of MapAlgebra operations */
trait Expression extends Product with Serializable {
  def children: Seq[Expression]
  def kind: MamlKind

  def sources: Seq[Source] = children.flatMap(_.sources).distinct
  def subExpression(expressionPath: ExpressionPath): Option[Expression] =
    expressionPath.indices.foldLeft(Option(this))({
      case (ast: Option[Expression], i: Int) => ast.flatMap({ _.children.lift(i) })
    })
}

