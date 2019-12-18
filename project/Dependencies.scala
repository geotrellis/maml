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
  private def ver(v211: String, v212: String) = Def.setting {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 11)) => v211
      case Some((2, 12)) => v212
      case v => sys.error(s"scala version: $v")
    }
  }

  def circe(module: String) = Def.setting {
    module match {
      case "optics" => "io.circe" %% s"circe-$module" % ver("0.11.0", "0.12.0").value
      case _        => "io.circe" %% s"circe-$module" % ver("0.11.1", "0.12.2").value }
  }

  def cats(module: String) = Def.setting {
    module match {
      case "core"   => "org.typelevel" %% s"cats-$module" % ver("1.6.1", "2.0.0").value
      case "effect" => "org.typelevel" %% s"cats-$module" % ver("1.3.1", "2.0.0").value
    }
  }

  def spark(module: String) = Def.setting {
    "org.apache.spark" %% s"spark-$module" % "2.4.4"
  }

  def geotrellis(module: String) = Def.setting {
    "org.locationtech.geotrellis" %% s"geotrellis-$module" % "3.2.0"
  }

  val logging    = "org.log4s"                  %% "log4s"                % "1.8.2"
  val scalatest  = "org.scalatest"              %% "scalatest"            % "3.0.1"
  val scalacheck = "org.scalacheck"             %% "scalacheck"           % "1.13.4"
}
