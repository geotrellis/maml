package maml.ast

import io.circe.generic.JsonCodec


case class ExpressionPath(indices: Seq[Int])

/** The ur-type for a recursive representation of MapAlgebra operations */
trait Expression extends Product with Serializable {
  def children: List[Expression]
  def sources: List[Source]
  def kind: MamlKind

  def subExpression(expressionPath: ExpressionPath): Option[Expression] =
    expressionPath.indices.foldLeft(Option(this))({
      case (ast: Option[Expression], i: Int) => ast.flatMap({ _.children.lift(i) })
    })
}

