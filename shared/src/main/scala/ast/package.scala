package maml

import maml.ast.utility._
import maml.error.InterpreterError

import io.circe._
import io.circe.optics.JsonPath._
import cats.data._
import Validated._
import scala.util.Try

package object ast {
  implicit val decodeKeyDouble: KeyDecoder[Double] = new KeyDecoder[Double] {
    final def apply(key: String): Option[Double] = Try(key.toDouble).toOption
  }
  implicit val encodeKeyDouble: KeyEncoder[Double] = new KeyEncoder[Double] {
    final def apply(key: Double): String = key.toString
  }
}
