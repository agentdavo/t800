package transputer

import spinal.core._
import spinal.lib.misc.database.Database
import transputer.Global._

/** Minimal instantiation of the Transputer core using only the essential
  * pipeline and configuration plugins. This intentionally omits the rest of the
  * standard plugin stack to demonstrate the bare compilation path.
  */
class BareBones extends Component {
  // Instantiate the core using the minimal plugin stack
  val db = Transputer.defaultDatabase()
  val core = Database(db).on(Transputer(Transputer.unitPlugins()))
}

object BareBonesVerilog {
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new BareBones)
  }
}
