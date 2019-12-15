package value

class Notification(val msg: String) extends Value {
  override def toString = msg
}

object Notification {
  def apply(msg: String) = new Notification(msg)
  val OK = Notification("OK")
  val DONE = Notification("DONE")
  val UNSPECIFIED = Notification("UNSPECIFIED")
}