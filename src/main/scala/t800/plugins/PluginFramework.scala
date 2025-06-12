package t800.plugins

import scala.collection.mutable
import scala.reflect.ClassTag

class PluginHost {
  private val services = mutable.Map[Class[_], Any]()
  def add[T](srv: T)(implicit ct: ClassTag[T]): Unit = services(
    ct.runtimeClass
  ) = srv
  def service[T](implicit ct: ClassTag[T]): T =
    services(ct.runtimeClass).asInstanceOf[T]

  def asHostOf(plugins: Seq[FiberPlugin]): Unit = {
    plugins.foreach(_.host = this)
    plugins.foreach(_.setup())
    plugins.foreach(_.build())
  }
}

trait FiberPlugin {
  var host: PluginHost = _
  def setup(): Unit = {}
  def build(): Unit = {}
  def addService[T](srv: T)(implicit ct: ClassTag[T]): Unit = host.add(srv)
}

object Plugin {
  def apply[T](implicit host: PluginHost, ct: ClassTag[T]): T = host.service[T]
}
