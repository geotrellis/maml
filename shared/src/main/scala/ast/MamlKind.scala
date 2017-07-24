package maml.ast

trait MamlKind
object MamlKind {
  case object MamlTile extends MamlKind
  case object MamlVector extends MamlKind
  case object MamlScalar extends MamlKind
}
