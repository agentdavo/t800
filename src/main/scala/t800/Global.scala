package t800

import spinal.core._
import scala.collection.mutable

/** Global database for build-time parameters. */
class Database {
  private val storage = mutable.LinkedHashMap[Database.Element[_], Any]()

  def update[T](key: Database.Element[T], value: T): Unit = key.set(this, value)
  def apply[T](key: Database.Element[T]): T = key.get(this)

  def storageUpdate[T](key: Database.Element[T], value: T): Unit =
    storage.update(key, value)
  def storageGet[T](key: Database.Element[T]): T =
    storage(key).asInstanceOf[T]
  def storageGetElseUpdate[T](key: Database.Element[T], create: => T): T =
    storage.getOrElseUpdate(key, create).asInstanceOf[T]
  def storageExists[T](key: Database.Element[T]): Boolean = storage.contains(key)
}

object Database extends ScopeProperty[Database] {
  def on[T](body: => T): T = this(new Database).on(body)
  def value[T](): Database.ElementValue[T] = new Database.ElementValue[T]()

  abstract class Element[T](sp: ScopeProperty[Database] = Database) extends Nameable {
    def get(): T = get(sp.get)
    def apply(): T = get(sp.get)
    def set(value: T): Unit = set(sp.get, value)
    def get(db: Database): T
    def set(db: Database, value: T): Unit
  }

  class ElementValue[T](sp: ScopeProperty[Database] = Database) extends Element[T](sp) {
    def get(db: Database): T = db.storageGet(this)
    def set(db: Database, value: T): Unit = db.storageUpdate(this, value)
  }
}

/** Shared build-time parameters. */
object Global extends AreaObject {
  import Database._

  val WORD_BITS: ElementValue[Int] = Database.value[Int]()
  val HART_COUNT: ElementValue[Int] = Database.value[Int]()
}
