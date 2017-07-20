package maml.scalar

trait Scalar {
  def evaluate: Int
  def evaluteDouble = Double
}

case class ScalarDouble(value: Double) extends Scalar {
  def evaluate: Int = value.toInt
  def evaluateDouble: Double = value
}

case class ScalarInt(value: Int) extends Scalar {
  def evaluate: Int = value
  def evaluateDouble: Double = value.toDouble
}
