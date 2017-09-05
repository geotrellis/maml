package maml.error

import maml.ast._


case class EvalTypeError(found: String, expected: List[String]) extends InterpreterError {
  def repr: String = s"Expected to evaluate tree as one of $expected; instead found $found"
}

case class S3TileResolutionError[T](exp: Expression[T], coords: Option[(Int, Int, Int)]) extends InterpreterError {
  def repr: String = coords match {
    case Some((z, x, y)) => s"Tile not found for $exp at SpatialKey $z, $x, $y}"
    case None => s"Tile not found for $exp"
  }
}

case class UnknownTileResolutionError[T](exp: Expression[T], coords: Option[(Int, Int, Int)]) extends InterpreterError {
  def repr: String = coords match {
    case Some((z, x, y)) => s"Unknown retrieval error for $exp at SpatialKey $z, $x, $y}"
    case None => s"Unkown retrieval error for $exp"
  }
}
