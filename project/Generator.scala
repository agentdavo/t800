sealed trait Generator
object Generator {
  case object Unit extends Generator
  case object BareBones extends Generator
}

import sbt._

object GeneratorKeys {
  val generator = settingKey[Generator]("Select source set for generation")
}
