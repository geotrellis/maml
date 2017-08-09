package maml.eval

trait LazyRep {
  def as[A](implicit mf: ClassManifest[A]): Interpreted[A]
}

