package com.azavea.maml.serve

import com.azavea.maml.ast._
import com.azavea.maml.eval._
import com.azavea.maml.util._
import com.azavea.maml.eval.directive._

import cats.data.NonEmptyList
import cats.data.Validated._
import io.circe._
import io.circe.syntax._
import geotrellis.raster._
import geotrellis.raster.render._
import geotrellis.spark._
import geotrellis.spark.io._
import geotrellis.spark.io.s3._
import akka.actor.ActorSystem
import akka.event.{LoggingAdapter, Logging}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshalling._
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.{ActorMaterializer, Materializer}
import akka.http.scaladsl.server.{Directives, ExceptionHandler}
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._
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

  Http().bindAndHandle(root, config.getString("http.interface"), config.getInt("http.port"))
}

trait Service extends InterpreterExceptionHandling {
  implicit def executor: ExecutionContextExecutor
  implicit val system: ActorSystem
  implicit val materializer: Materializer

  def pngAsHttpResponse(png: Png): HttpResponse =
    HttpResponse(entity = HttpEntity(ContentType(MediaTypes.`image/png`), png.bytes))

  val cMap = ColorRamps.Viridis

  val interpreter = Interpreter.buffering(
    ScopedDirective.pure[TileLiteral](SourceDirectives.tileLiteral),
    ScopedDirective.pure[IntLiteral](SourceDirectives.intLiteral),
    ScopedDirective.pure[FocalMax](FocalDirectives.focalMax),
    ScopedDirective.pure[Addition](OpDirectives.additionTile orElse OpDirectives.additionInt orElse OpDirectives.additionDouble),
    ScopedDirective.pure[Equal](OpDirectives.equalTo)
  )

  implicit def encodeNEL[A: Encoder]: Encoder[NonEmptyList[A]] = Encoder.encodeList[A].contramap[NonEmptyList[A]](_.toList)

  val resolver = new Resolver(executor)
  def root =
    pathPrefix(IntNumber / IntNumber / IntNumber) { (z, x, y) =>
      handleExceptions(interpreterExceptionHandler) {
        complete {
          val ast = Equal(List(Addition(List(ValueReaderTileSource("geopyspark-test", "srtm", "srtm-test"), IntLiteral(1))), ValueReaderTileSource("geopyspark-test", "srtm", "srtm-test")))
          val futureAst: Future[Interpreted[Expression]] = resolver.tmsLiteral(ast)(z, x, y)
          futureAst.map({ resolvedAst =>
            resolvedAst
              .andThen({ interpreter(_) })
              .andThen({ _.as[Tile] }) match {
                case Valid(tile) => pngAsHttpResponse(tile.renderPng(cMap))
                case Invalid(nel) => throw InterpreterException(nel)
              }
          })
        }
      }
    }
}
