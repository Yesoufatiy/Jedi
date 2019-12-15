package value

import expression.Literal

case class Chars(val scalaValue: String) extends Literal with Ordered[Chars] {
   
  def substring(start: Integer, end: Integer) = Chars(scalaValue.substring(start.value, end.value))
  def length: Integer = Integer(scalaValue.length)
  
  def +(other: Chars): Chars = Chars(this.scalaValue + other.scalaValue)
  override def toString = scalaValue
  def compare(other: Chars): Int = this.scalaValue.compare(other.scalaValue)
  override def equals(other: Any): Boolean = 
    other match {
       case other: Chars => (other.isInstanceOf[Chars]) && (other.scalaValue == this.scalaValue)
       case _ => false
    }
  override def hashCode = this.toString.##
}

object Chars {
  
}