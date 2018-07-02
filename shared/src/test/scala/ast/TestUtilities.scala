package com.azavea.maml.ast

import cats.effect.IO


object TestUtilities {
  case class TileSource(resolved: String = "42") extends UnboundSource {
    val kind = MamlKind.Tile
    val resolveBinding = IO.raiseError(new java.lang.IllegalArgumentException("This is a dummy Source used only for testing the type system"))
  }

  case class IntSource(resolved: Int = 42) extends UnboundSource {
    val kind = MamlKind.Int
    val resolveBinding = IO.pure(IntLiteral(resolved))
  }

  case class DoubleSource(resolved: Double = 42.0) extends UnboundSource {
    val kind = MamlKind.Double
    val resolveBinding = IO.pure(DoubleLiteral(resolved))
  }
}
