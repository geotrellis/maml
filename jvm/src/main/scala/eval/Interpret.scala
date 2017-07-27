package maml.eval

import maml.ast._

import cats._
import cats.data._
import cats.data.Validated._
import cats.implicits._


object Interpret {
  /** The Interpreted type is either a list of failures or a compiled MapAlgebra operation */
  type Interpreted[A] = ValidatedNel[InterpreterError, A]

  /** Interpret an AST with its matched execution parameters, but do so
    * without fetching any Rasters. Only interprets the structural validatity of
    * the AST, given the params.
    */
  def pure[M: Monoid](
    expr: Expression
  ): Interpreted[M] = expr match {
    /* Validate leaf nodes */
    case TileSource => Valid(Monoid.empty)
    case ScalarSource(_) => Valid(Monoid.empty)
    case VectorSource => Valid(Monoid.empty)

    case op: UnaryExpression => {
      /* Check for errors further down, first */
      val kids: Interpreted[M] = op.children.foldMap(a => pure(a))
      val argLen: Interpreted[M] = Valid(Monoid.empty)

      /* Combine these (potential) errors via their Semigroup instance */
      kids.combine(argLen)
    }

    case op: FoldableExpression => {
      val kids: Interpreted[M] = op.children.foldMap(a => pure(a))
      val argLen: Interpreted[M] = Valid(Monoid.empty)
      kids.combine(argLen)
    }
  }
}
