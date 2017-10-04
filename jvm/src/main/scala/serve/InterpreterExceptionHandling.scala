package com.azavea.maml.serve

import com.azavea.maml.eval._

import io.circe._
import io.circe.syntax._
import cats.data._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Directives, ExceptionHandler}
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._


trait InterpreterExceptionHandling extends Directives {
  val interpreterExceptionHandler = ExceptionHandler {
    case ie: InterpreterException =>
      complete{ (StatusCodes.BadRequest, ie.errors) }
  }
}

