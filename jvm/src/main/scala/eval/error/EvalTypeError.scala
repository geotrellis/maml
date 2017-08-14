package maml.error

case class EvalTypeError(found: String, expected: List[String]) extends InterpreterError {
  def repr: String = s"Expected to evaluate tree as one of $expected; instead found $found"
}
