package com.azavea.maml.ast

import com.azavea.maml.util._
import geotrellis.raster.TargetCell

import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary.arbitrary

object Generators {

  lazy val genScalarSourceAST: Gen[Expression] = for {
    int <- arbitrary[Int]
    dbl <- arbitrary[Double]
    str <- arbitrary[String]
    src <- Gen.oneOf(IntLit(int), DblLit(dbl), RasterVar(str), DblVar(str), IntVar(str))
  } yield src

  def genBinaryOpAST(depth: Int) = for {
    constructor <- Gen.lzy(
      Gen.oneOf(
        Addition.apply _,
        Subtraction.apply _,
        Multiplication.apply _,
        Division.apply _,
        Max.apply _,
        Min.apply _,
        Lesser.apply _,
        LesserOrEqual.apply _,
        Equal.apply _,
        GreaterOrEqual.apply _,
        Greater.apply _
      )
    )
    args <- containerOfN[List, Expression](2, genExpression(depth))
  } yield constructor(args)

  def genClassificationAST(depth: Int) = for {
    args <- containerOfN[List, Expression](1, genExpression(depth))
  } yield Classification(args, ClassMap(Map(1.0 -> 1, 2.0 -> 200)))

  def genMaskingAST(depth: Int) = for {
    args <- containerOfN[List, Expression](1, genExpression(depth))
  } yield Masking(args)

  def genFocalOpAST(depth: Int) = for {
    constructor <- Gen.lzy(
      Gen.oneOf(
        FocalMax.apply _,
        FocalMin.apply _,
        FocalStdDev.apply _,
        FocalMean.apply _,
        FocalMedian.apply _,
        FocalMode.apply _,
        FocalSum.apply _
      )
    )
    args <- containerOfN[List, Expression](1, genExpression(depth))
    neighborhood <- Gen.oneOf(
      Square(123),
      Circle(123.4),
      Nesw(123),
      Wedge(42.2, 45.1, 51.3),
      Annulus(123.0, 123.4)
    )
  } yield constructor(args, neighborhood, TargetCell.All)

  def genOpAST(depth: Int) = Gen.frequency(
    5 -> genBinaryOpAST(depth),
    2 -> genMaskingAST(depth),
    1 -> genClassificationAST(depth),
    2 -> genFocalOpAST(depth)
  )

  /* We are forced to manually control flow in this generator to prevent stack overflows
   *  See: http://stackoverflow.com/questions/19829293/scalacheck-arbitrary-implicits-and-recursive-generators
   */
  def genExpression(depth: Int = 1): Gen[Expression] =
    if (depth >= 100) genScalarSourceAST
    else Gen.frequency(1 -> genBinaryOpAST(depth + 1), 1 -> genScalarSourceAST)
}
