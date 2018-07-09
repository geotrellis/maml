package com.azavea.maml.error

import cats.data.NonEmptyList


case class MamlException(errors: NonEmptyList[MamlError]) extends Exception

