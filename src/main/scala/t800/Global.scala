package t800

import spinal.core._

/** Global configuration elements accessed via [[Database]]. Plugins should use these handles rather
  * than constants in [[TConsts]].
  */
object Global extends AreaObject {
  val WORD_BITS = Database.blocking[Int]
  val ADDR_BITS = Database.blocking[Int]
  val ROM_WORDS = Database.blocking[Int]
  val RAM_WORDS = Database.blocking[Int]
  val LINK_COUNT = Database.blocking[Int]
  val RESET_PC = Database.blocking[Long]
}
