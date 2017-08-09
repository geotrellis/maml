package maml.ast

import MamlKind._
import maml.ast.utility._

import java.security.InvalidParameterException


/** Operations which should only have one argument. */
trait BinaryExpression extends Expression {
  require(children.length == 2, s"Incorrect number of arguments to a binary expression. Expected 2, found ${children.length}")
  val kindDerivation: (MamlKind, MamlKind) => MamlKind
  lazy val kind = this.children.map({ _.kind }).reduce({ kindDerivation(_, _) })
}

case class Masking(children: List[Expression]) extends Operation with BinaryExpression {
  val kindDerivation = { (k1: MamlKind, k2: MamlKind) =>
    (k1, k2) match {
      case (MamlKind.Tile, MamlKind.Vector) => MamlKind.Tile
      case (MamlKind.Vector, MamlKind.Tile) => MamlKind.Tile
      case (x1, x2) => throw new InvalidParameterException(s"Expected tile and vector kinds. Found $x1 and $x2")
    }
  }

  def withChildren(newChildren: List[Expression]): Expression = copy(children = newChildren)
}
