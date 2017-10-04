package com.azavea.maml.eval

import cats.data.NonEmptyList


case class InterpreterException(errors: NonEmptyList[InterpreterError]) extends Exception

