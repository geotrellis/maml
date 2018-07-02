package com.azavea.maml.ast


trait Operation extends Expression {
  lazy val sources: Set[Source] =
    children
      .flatMap(_.sources)
      .toSet

  lazy val unboundSources: Set[UnboundSource] =
    children
      .flatMap(_.unboundSources)
      .toSet

  lazy val boundSources: Set[BoundSource] =
    children
      .flatMap(_.boundSources)
      .toSet
}

