package com.azavea.maml

import cats.data._
import cats.data.Validated._

package object error {
  type Interpreted[A] = ValidatedNel[MamlError, A]
}
