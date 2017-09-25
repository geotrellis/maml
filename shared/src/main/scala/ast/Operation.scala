package com.azavea.maml.ast


trait Operation extends Expression {
  lazy val sources: List[Source] =
    children
      .flatMap(_.sources)
      .distinct
}

