package maml.ast.codec.tree

import maml.ast._
import ExpressionTreeCodec._

import cats.syntax.either._
import io.circe.syntax._
import org.scalacheck.Prop.forAll
import org.scalatest._
import org.scalatest.prop._


class ExpressionTreeCodecSpec extends PropSpec with Checkers {
  property("bijective serialization on whole tree") {
    check(forAll(Generators.genExpression()) { (ast: Expression) =>
      ast.asJson.as[Expression] match {
        case Left(f) => {println(f);fail(f)}
        case Right(exp) => exp == ast
      }
    })
  }
}
