package maml.rdd

import maml.ast._
import maml.eval._
import maml.eval.directive._
import maml.eval.directive.SourceDirectives._
import maml.dsl.jvm._

import maml.rdd.eval._
import maml.rdd.eval.directive._
import maml.rdd.eval.SpatialRDDResult._
import maml.rdd.ast._

import geotrellis.raster._
import geotrellis.raster.testkit._
import geotrellis.spark._
import geotrellis.spark.mask._
import geotrellis.spark.testkit._
import geotrellis.vector._

import cats._
import cats.data.{NonEmptyList => NEL, _}
import Validated._

import org.scalatest._
import org.apache.spark._
import org.apache.spark.rdd._

import scala.reflect._
import scala.util.Random


class RDDMaskSpec extends FunSpec
  with Matchers
  with RasterMatchers
  with TestEnvironment {

  val interpreter = Interpreter.naive(
    intLiteralDirective,
    RDDSourceDirectives.spatialRDDLiteralDirective,
    RDDMaskDirectives.maskDirective
  )

  implicit class TypeRefinement(self: Interpreted[Result]) {
    def as[T: ClassTag]: Interpreted[T] = self match {
      case Valid(r) => r.as[T]
      case i@Invalid(_) => i
    }
  }

  it("should mask a layer with an Extent") {
    val arr = (1 to (6*4)).map(_.toFloat).toArray
    arr(17) = -1.1f
    val sourceTile = FloatArrayTile(arr, 6, 4, FloatUserDefinedNoDataCellType(-1.1f))

    val (tile, layer) = createTileLayerRDD(sourceTile, 2, 2)

    val Extent(xmin, ymin, xmax, ymax) = layer.metadata.extent
    val dx = (xmax - xmin) / 3
    val dy = (ymax - ymin) / 2
    val mask = Extent(xmin + dx, ymin, xmax, ymin + dy)
    val n = -1.1f

    val interpreted =
      interpreter(RDDMask(List(SpatialRDDLiteral(layer)), mask)).as[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]]

    val expected = FloatArrayTile(
      Array(
        n, n, 15, 16, 17, n,
        n, n, 21, 22, 23, 24
      ), 6, 2, FloatUserDefinedNoDataCellType(-1.1f))

    interpreted match {
      case Valid(result) => assertEqual(result.stitch.tile, expected)
      case i@Invalid(_) => print(s"$i")
    }
  }

  describe("should mask a layer with Multiple Polygons") {
    val arr = Array.fill[Int](24)(1)
    val sourceTile = IntArrayTile(arr, 6, 4)
    val (_, layer) = createTileLayerRDD(sourceTile, 2, 2)
    val tile = layer.stitch.tile
    val extent = layer.metadata.extent
    val height = extent.height.toInt
    val width = extent.width.toInt

    def inMirror(bound: Int): Int = inRange(-bound to bound)
    def inRange(bounds: Range): Int = Random.nextInt(bounds.max - bounds.min) + bounds.min

    def triangle(size: Int, dx: Int, dy: Int): Line =
      Line(Seq[(Double, Double)]((0, 0), (size, 0), (size, size), (0, 0))
        .map { case (x, y) => (x + dx, y + dy) })

    def randomPolygons(number: Int)(maxWidth: Int, maxHeight: Int): Seq[Polygon] = {
      val max = Math.min(maxWidth, maxHeight)
      val min = max / 10
      for {
        _ <- 1 to number
        size = inRange(min to max)
        placeLeft = Math.max(0, max - size)
        dx = inMirror(placeLeft) - size / 2
        dy = inMirror(placeLeft) - size / 2
        border = triangle(size, dx, dy)
        hole = triangle(size / 3, dx + size / 2, dy + size / 3)
      } yield Polygon(border, hole)
    }

    val opts = Mask.Options(filterEmptyTiles = false)

    it("should be masked by random polygons") {
      randomPolygons(50)(width, height) foreach { poly =>
        if (poly.isValid) {
          val interpreted = interpreter(RDDMask(List(SpatialRDDLiteral(layer)), poly, opts)).as[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]]
          val expected = tile.mask(extent, poly)

          interpreted match {
            case Valid(result) => result.stitch.toArray() shouldEqual expected.toArray()
            case i@Invalid(_) => println("$i")
          }
        }
      }
    }

    it("should be masked by random multiPolygons") {
      val polygons = randomPolygons(50)(width, height)
      val multiPolygons = polygons.zip(polygons.reverse).map { case (a, b) =>
        MultiPolygon(a, b)
      }

      multiPolygons.foreach { poly =>
        if (poly.isValid) {
          val interpreted = interpreter(RDDMask(List(SpatialRDDLiteral(layer)), poly, opts)).as[ContextRDD[SpatialKey, Tile, TileLayerMetadata[SpatialKey]]]
          val expected = tile.mask(extent, poly)

          interpreted match {
            case Valid(result) => result.stitch.toArray() shouldEqual expected.toArray()
            case i@Invalid(_) => println("$i")
          }
        }
      }
    }
  }
}
