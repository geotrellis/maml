package com.azavea.maml.ast.codec.tree

import com.azavea.maml.ast._

import org.log4s._
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import cats.syntax.either._
import org.scalacheck.Prop.forAll
import org.scalatest._
import org.scalatest.prop._
import propspec._

class ExpressionTreeCodecSpec extends AnyPropSpec with ExpressionTreeCodec {
  @transient private[this] lazy val logger = getLogger

  property("bijective serialization on whole tree") {
    forAll(Generators.genExpression()) { (ast: Expression) =>
      logger.debug(s"Attempting to encode AST: $ast")
      val encoded = ast.asJson.noSpaces
      logger.debug(s"Encoded AST: $encoded")
      val decoded = decode[Expression](encoded)
      logger.debug(s"Decoded AST: $decoded")
      decoded match {
        case Right(exp) =>
          exp == ast
        case Left(f) =>
          logger.debug(f)(f.toString)
          fail(f)
      }
    }
  }
}
