package maml.ast


trait Operation[T] extends Expression[T] {
  lazy val sources: List[Source[T]] =
    children
      .flatMap(_.sources)
      .distinct
}
