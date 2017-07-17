package maml.ast

import maml.ast.metadata._
import maml.ast.utility._

import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll

import scala.util.Random
import java.util.UUID


object Generators {

  implicit lazy val arbUUID: Arbitrary[UUID] = Arbitrary(UUID.randomUUID)

  implicit lazy val arbHistogram: Arbitrary[Histogram] = Arbitrary {
    Histogram(Map(1.0 -> 100, 2.0 -> 300))
  }

  lazy val genClassMapOptions: Gen[ClassMap.Options] = for {
    bounds <- Gen.lzy(Gen.oneOf(LessThanOrEqualTo, LessThan, Exact, GreaterThan, GreaterThanOrEqualTo))
    ndVal <- arbitrary[Int]
    fallback <- arbitrary[Int]
  } yield ClassMap.Options(bounds, ndVal, fallback)

  lazy val genClassMap: Gen[ClassMap] = for {
    dubs <- Gen.containerOfN[List, Double](30, arbitrary[Double])
    ints <- Gen.containerOfN[List, Int](30, arbitrary[Int])
  } yield ClassMap(dubs.zip(ints).toMap)

  /*
  lazy val genMultiPolygon: Gen[MultiPolygon] = for {
    xs <- Gen.containerOfN[List, Double](10, arbitrary[Double])
    ys <- Gen.containerOfN[List, Double](10, arbitrary[Double])
  } yield MultiPolygon(Polygon(xs.zip(ys) :+ (xs.head, ys.head)))
   */

  lazy val genNodeMetadata: Gen[NodeMetadata] = for {
    label <- Gen.option(arbitrary[String])
    desc  <- Gen.option(arbitrary[String])
    hist  <- Gen.option(arbitrary[Histogram])
    cRamp <- Gen.lzy(Gen.option(Vector(1, 2, 3)))
    cMap  <- Gen.option(genClassMap)
  } yield NodeMetadata(label, desc, hist, cRamp, cMap)

  lazy val genTileSourceAST = for {
    id <- arbitrary[UUID]
    nmd <- Gen.option(genNodeMetadata)
  } yield TileSource(id, nmd)

  lazy val genScalarSourceAST = for {
    id <- arbitrary[UUID]
    const <- arbitrary[Int]
    nmd <- Gen.option(genNodeMetadata)
  } yield ScalarSource(id, const, nmd)

  def genBinaryOpAST(depth: Int) = for {
    constructor <- Gen.lzy(Gen.oneOf(
                     Addition.apply _,
                     Subtraction.apply _,
                     Multiplication.apply _,
                     Division.apply _,
                     Max.apply _,
                     Min.apply _
                   ))
    args <- containerOfN[List, MamlTree](2, genMamlTree(depth))
    id <- arbitrary[UUID]
    nmd <- Gen.option(genNodeMetadata)
  } yield constructor(args, id, nmd)

  def genClassificationAST(depth: Int) = for {
    args <- containerOfN[List, MamlTree](1, genMamlTree(depth))
    id <- arbitrary[UUID]
    nmd <- Gen.option(genNodeMetadata)
    cmap <- genClassMap
  } yield Classification(args, id, nmd, cmap)

  def genMaskingAST(depth: Int) = for {
    args <- containerOfN[List, MamlTree](1, genMamlTree(depth))
    id <- arbitrary[UUID]
    nmd <- Gen.option(genNodeMetadata)
  } yield {
    val mp = MultiPolygon(Array(Polygon(Array(Point(0, 0), Point(0, 10), Point(10, 10), Point(10, 0), Point(0, 0)))))

    Masking(args, id, nmd, mp)
  }

  def genFocalOpAST(depth: Int) = for {
    constructor  <- Gen.lzy(Gen.oneOf(
                      FocalMax.apply _,
                      FocalMin.apply _,
                      FocalStdDev.apply _,
                      FocalMean.apply _,
                      FocalMedian.apply _,
                      FocalMode.apply _,
                      FocalSum.apply _
                    ))
    args         <- containerOfN[List, MamlTree](1, genMamlTree(depth))
    id           <- arbitrary[UUID]
    nmd          <- Gen.option(genNodeMetadata)
    neighborhood <- Gen.oneOf(
                      Square(123),
                      Circle(123.4),
                      Nesw(123),
                      Wedge(42.2, 45.1, 51.3),
                      Annulus(123.0, 123.4)
                    )
  } yield constructor(args, id, nmd, neighborhood)

  // TODO: If `genMaskingAST` is included, AST generation diverges!
  def genOpAST(depth: Int) = Gen.frequency(
    (5 -> genBinaryOpAST(depth)),
//    (2 -> genMaskingAST(depth)),
    (1 -> genClassificationAST(depth)),
    (2 -> genFocalOpAST(depth))
  )

  def genLeafAST = Gen.oneOf(genScalarSourceAST, genTileSourceAST)

  /** We are forced to manually control flow in this generator to prevent stack overflows
    *  See: http://stackoverflow.com/questions/19829293/scalacheck-arbitrary-implicits-and-recursive-generators
    */
  def genMamlTree(depth: Int = 1): Gen[MamlTree] =
    if (depth >= 100) genLeafAST
    else Gen.frequency((1 -> genOpAST(depth + 1)), (1 -> genLeafAST))
}
