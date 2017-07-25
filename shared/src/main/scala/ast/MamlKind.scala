package maml.ast

trait MamlKind
object MamlKind {
  case object Tile extends MamlKind
  case object Vector extends MamlKind
  case object Scalar extends MamlKind
}
