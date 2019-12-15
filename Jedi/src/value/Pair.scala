package value

class Pair(val first: Value, val second: Value) extends Value {
  override def toString = "(" + first + ", " + second + ")"
}