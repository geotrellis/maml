package maml.serve

import maml.ast._
import maml.ast.utility._
import maml.ast.jvm._
import maml.eval._
import maml.eval.directive._

import geotrellis.raster._
import geotrellis.raster.render._
import geotrellis.spark._
import geotrellis.spark.io._
import geotrellis.spark.io.s3._

import cats.data.Validated._
import akka.actor.ActorSystem
import akka.event.{LoggingAdapter, Logging}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.model.{HttpResponse, ContentType, HttpEntity, MediaTypes}
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.{ActorMaterializer, Materializer}

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

import scala.concurrent._


object MamlService extends App with Service {
  override implicit val system = ActorSystem()
  override implicit val executor = system.dispatcher
  override implicit val materializer = ActorMaterializer()

  val config = ConfigFactory.load()
  val logger = Logging(system, getClass)

  val valueReader = S3ValueReader("geopyspark-test", "srtm")
  val cMap = ColorRamps.Viridis

  val interpreter = Interpreter.buffering(
    ScopedDirective.pure[TileLiteral](SourceDirectives.tileLiteralDirective),
    ScopedDirective.pure[FocalMax](FocalDirectives.focalMaxDirective)
  )

  Http().bindAndHandle(root, config.getString("http.interface"), config.getInt("http.port"))
}

trait Service {
  implicit def executor: ExecutionContextExecutor
  implicit val system: ActorSystem
  implicit val materializer: Materializer

  def pngAsHttpResponse(png: Png): HttpResponse =
    HttpResponse(entity = HttpEntity(ContentType(MediaTypes.`image/png`), png.bytes))

  def root =
    pathPrefix(IntNumber / IntNumber / IntNumber) { (z, x, y) =>
      rejectEmptyResponse {
        complete {
          Future {
            val tileOpt: Option[Tile] =
              try {
                Some(
                  MamlService.valueReader.reader[SpatialKey, Tile](
                    LayerId("srtm-test", z)
                  ).read(x, y)
                )
              } catch {
                case _: ValueNotFoundError => None
              }
              tileOpt.flatMap { tile =>
                MamlService.interpreter(FocalMax(List(TileLiteral(tile)), Square(2)))
                  .as[Tile]
                  .toOption
                  .map { t =>
                    pngAsHttpResponse(t.renderPng(MamlService.cMap))
                }
              }
          }
        }
      }
    }
}
