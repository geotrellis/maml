package com.azavea.maml.ast.codec.tree

import com.azavea.maml.ast._
import ExpressionTreeCodec._

import io.circe._
import io.circe.syntax._
import cats.syntax.either._
import org.scalacheck.Prop.forAll
import org.scalatest._
import org.scalatest.prop._

// TODO uncomment generator code later...
//class ExpressionTreeCodecSpec extends PropSpec with Checkers {
//  property("bijective serialization on whole tree") {
//    println(DoubleLiteral(4.032918932483637E-82).asJson.noSpaces)
//    println(List(DoubleLiteral(4.032918932483637E-82), DoubleLiteral(-3.717117209923295E119)).asJson)
//    println(Min(List(DoubleLiteral(4.032918932483637E-82), DoubleLiteral(-3.717117209923295E119))).asJson)
//    check(forAll(Generators.genExpression()) { (ast: Expression) =>
//      println("HERE LOOK", ast)
//      ast.asJson.as[Expression] match {
//        case Right(exp) => exp == ast
//        case Left(f) => {println(f);fail(f)}
//      }
//    })
//  }
//}
