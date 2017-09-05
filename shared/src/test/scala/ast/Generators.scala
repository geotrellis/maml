package maml.ast

import maml.ast.utility._

import cats._
import cats.implicits._
import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll

import scala.util.Random
import java.util.UUID


object Generators {

  lazy val genTileSourceAST: Gen[TileSource[Unit]] = for {
    str <- arbitrary[String]
  } yield TileSource(str, Monoid.empty[Unit])

  lazy val genScalarSourceAST: Gen[Source[Unit]] = for {
    int <- arbitrary[Int]
    dbl <- arbitrary[Double]
    str <- arbitrary[String]
    src <- Gen.lzy(
      Gen.oneOf(
        IntSource(str, Monoid.empty[Unit]),
        DoubleSource(str, Monoid.empty[Unit]),
        IntLiteral(int, Monoid.empty[Unit]),
        DoubleLiteral(dbl, Monoid.empty[Unit])))
  } yield src

  def genBinaryOpAST(depth: Int): Gen[Expression[Unit]] = for {
    constructor <- Gen.lzy(
      Gen.oneOf(
        { (args: List[Expression[Unit]], extra: Unit) => Addition.apply(args, extra) },
        { (args: List[Expression[Unit]], extra: Unit) => Subtraction.apply(args, extra) },
        { (args: List[Expression[Unit]], extra: Unit) => Multiplication.apply(args, extra) },
        { (args: List[Expression[Unit]], extra: Unit) => Division.apply(args, extra) },
        { (args: List[Expression[Unit]], extra: Unit) => Max.apply(args, extra) },
        { (args: List[Expression[Unit]], extra: Unit) => Min.apply(args, extra) },
        { (args: List[Expression[Unit]], extra: Unit) => Less.apply(args, extra) },
        { (args: List[Expression[Unit]], extra: Unit) => LessOrEqual.apply(args, extra) },
        { (args: List[Expression[Unit]], extra: Unit) => Equal.apply(args, extra) },
        { (args: List[Expression[Unit]], extra: Unit) => GreaterOrEqual.apply(args, extra) },
        { (args: List[Expression[Unit]], extra: Unit) => Greater.apply(args, extra) }
      ))
    args <- containerOfN[List, Expression[Unit]](2, genExpression(depth))
  } yield constructor(args, Monoid.empty[Unit])

  //def genClassificationAST(depth: Int) = for {
  //  args <- containerOfN[List, Expression](1, genExpression(depth))
  //  cmap <- genClassMap
  //} yield Classification(args, cmap)

//  def genMaskingAST(depth: Int) = for {
//    args <- containerOfN[List, Expression](1, genExpression(depth))
//  } yield {
//    val mp = MultiPolygon(Array(Polygon(Array(Point(0, 0), Point(0, 10), Point(10, 10), Point(10, 0), Point(0, 0)))))

//    Masking(args)
//  }

//  def genFocalOpAST(depth: Int) = for {
//    constructor  <- Gen.lzy(Gen.oneOf(
//                      FocalMax.apply _,
//                      FocalMin.apply _,
//                      FocalStdDev.apply _,
//                      FocalMean.apply _,
//                      FocalMedian.apply _,
//                      FocalMode.apply _,
//                      FocalSum.apply _
//                    ))
//    args         <- containerOfN[List, Expression](1, genExpression(depth))
//    id           <- arbitrary[UUID]
//    neighborhood <- Gen.oneOf(
//                      Square(123),
//                      Circle(123.4),
//                      Nesw(123),
//                      Wedge(42.2, 45.1, 51.3),
//                      Annulus(123.0, 123.4)
//                    )
//  } yield constructor(args, neighborhood)

//  // TODO: If `genMaskingAST` is included, AST generation diverges!
//  def genOpAST(depth: Int) = Gen.frequency(
//    (5 -> genBinaryOpAST(depth)),
//    //(2 -> genMaskingAST(depth)),
//    (1 -> genClassificationAST(depth)),
//    (2 -> genFocalOpAST(depth))
//  )

  def genLeafAST = Gen.oneOf(genScalarSourceAST, genTileSourceAST)

  /* We are forced to manually control flow in this generator to prevent stack overflows
   *  See: http://stackoverflow.com/questions/19829293/scalacheck-arbitrary-implicits-and-recursive-generators
   */
  def genExpression(depth: Int = 1): Gen[Expression[Unit]] =
    if (depth >= 100) genLeafAST
    else Gen.frequency((1 -> genBinaryOpAST(depth + 1)), (1 -> genLeafAST))
}
