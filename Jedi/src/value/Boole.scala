package value

import expression.Literal

case class Boole (val value: Boolean) extends Literal with Equals {
  def &&(other: Boole) = Boole(value && other.value)
  def ||(other: Boole) = Boole(value || other.value)
  def unary_! = Boole(!value)
  override def toString = value.toString
  override def canEqual(other: Any) =  other.isInstanceOf[Boole]
  override def equals(other: Any): Boolean = 
    other match {
       case other: Boole => this.canEqual(other) && (other.value == this.value)
       case _ => false
    }
  override def hashCode = this.toString.##
}

object Boole {
  val FALSE = Boole(false)
  val TRUE = Boole(true)
}