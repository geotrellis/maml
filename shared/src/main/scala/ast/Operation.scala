package maml.ast


trait Operation extends Expression {
  lazy val sources: List[Source] = children.flatMap(_.sources).distinct
}

