package t800

import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.misc.database.{Database, ElementLanda}

class DataBaseElementSpec extends AnyFunSuite {
  test("ElementValue isEmpty tracks storage") {
    val db = new Database
    val e = Database.value[Int]()
    assert(e.isEmpty(db))
    e.set(db, 42)
    assert(!e.isEmpty(db))
  }

  test("ElementLanda set updates stored value") {
    var executed = false
    val e = Database.landa({ executed = true; 1 })
    val db = new Database
    e.set(db, 5)
    assert(!executed)
    assert(e.getOn(db) == 5)
    assert(!executed)
  }
}
