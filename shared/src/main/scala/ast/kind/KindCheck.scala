package maml.ast.kind

import maml.ast._
import maml.error._

import cats._
import cats.data._
import cats.implicits._
import Validated._


object KindCheck {
  def apply(exp: Expression): Interpreted[MamlKind] = exp match {
    case src: Source =>
      Valid(src.kind)
    case foldable: FoldableExpression =>
      foldable
        .children.map({ apply(_) })
        .reduce({ (kind1, kind2) =>
          kind1.andThen({ k1 => kind2.andThen({ k2 =>
            foldable.kindDerivation(k1, k2)
          }) })
        })
    case unary: UnaryExpression =>
      val childKind = apply(unary.children.head)
      childKind match {
        case Valid(kind) =>
          val maybeKind = unary.expectedKind.get(kind)
          if (maybeKind.isDefined) Valid(maybeKind.get)
          else Invalid(NonEmptyList.of(UnaryTypeError(unary, kind)))
        case i@Invalid(_) => i
      }
  }
}
