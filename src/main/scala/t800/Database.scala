package t800

import scala.collection.mutable
import spinal.core._
import spinal.core.fiber.Handle

/** Simple thread-local key/value store used for global configuration. */
class Database {
  // user API
  def update[T](key: Database.Element[T], value: T): Unit = key.set(this, value)
  def apply[T](key: Database.Element[T]): T = key.get(this)

  // internal storage
  private val storage = mutable.LinkedHashMap[Database.Element[_ <: Any], Any]()
  def storageUpdate[T](key: Database.Element[T], value: T): Unit = storage.update(key, value)
  def storageGet[T](key: Database.Element[T]): T = storage(key).asInstanceOf[T]
  def storageGetElseUpdate[T](key: Database.Element[T], create: => T): T =
    storage.getOrElseUpdate(key, create).asInstanceOf[T]
  def storageExists[T](key: Database.Element[T]): Boolean = storage.contains(key)
}

object Database extends ScopeProperty[Database] {
  def on[T](body: => T): T = this(new Database).on(body)
  def value[T](): Database.ElementValue[T] = new ElementValue[T]()
  def blocking[T](): Database.ElementBlocking[T] = new ElementBlocking[T]()
  def landa[T](body: => T): Database.ElementLanda[T] = new ElementLanda[T](body)

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
  class ElementBlocking[T](sp: ScopeProperty[Database] = Database)
      extends Element[T](sp)
      with Area {
    private val thing = new ElementValue[Handle[T]]()
    def getHandle(db: Database): Handle[T] =
      db.storageGetElseUpdate(thing, new Handle[T].setCompositeName(this))
    def get(db: Database): T = getHandle(db).get
    def set(db: Database, value: T): Unit = {
      assert(!getHandle(db).isLoaded)
      getHandle(db).load(value)
    }
  }
  class ElementLanda[T](body: => T, sp: ScopeProperty[Database] = Database)
      extends ElementValue[T](sp) {
    override def get(db: Database): T = {
      if (!db.storageExists(this)) {
        db.storageUpdate(this, body)
      }
      super.get(db)
    }
    override def set(db: Database, value: T): Unit = ???
  }
}
