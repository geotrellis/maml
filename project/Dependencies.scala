/*
 * Copyright (c) 2019 Azavea.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import sbt._
import sbt.Keys._

object Dependencies {
  private def ver(v212: String, v213: String) = Def.setting {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) => v212
      case Some((2, 13)) => v213
      case v             => sys.error(s"scala version: $v")
    }
  }

  def circe(module: String) = Def.setting {
    val version = module match {
      case "generic-extras" => "0.14.3"
      case "optics"         => "0.14.1"
      case _                => "0.14.7"
    }

    "io.circe" %% s"circe-$module" % version
  }

  def cats(module: String) = Def.setting {
    val version = module match {
      case "core"   => "2.10.0"
      case "effect" => "3.5.4"
    }

    "org.typelevel" %% s"cats-$module" % version
  }

  def spark(module: String) = Def.setting {
    "org.apache.spark" %% s"spark-$module" % "3.5.1"
  }

  def geotrellis(module: String) = Def.setting {
    "org.locationtech.geotrellis" %% s"geotrellis-$module" % "3.7.1"
  }

  val logging = "org.log4s" %% "log4s" % "1.10.0"
  val scalatest = "org.scalatest" %% "scalatest" % "3.2.18"
  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.18.0"
}
