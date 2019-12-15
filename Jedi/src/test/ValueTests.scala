package test

import value._
import expression._
import context._


object ValueTests extends App {
  
  println("Testing chars ... ")
  
  val s1 = Chars("Bat")
  val s2 = Chars("man")
  val s3 = Chars(" is coming!")
  val s4 = s1 + s2 + s3
  val s5 = Chars("Batman is coming!")
  
  println("s4 = " + s4)
  
  println(s4.substring(Integer(3), Integer(9)))
  
  println(s1 + " < " + s3 + " = " + (s1 < s3))
  println(s1 + " == " + s5 + " = " + (s1 == s5))
  
  println("Testing numbers ...")
  var i1 = Integer(7)
  var i2 = Integer(6)
  var i3 = Integer(42)
  println("i1 - i2 = " + (i1 - i2))
  println("i1 * i2 = " + (i1 * i2))
  println("i1 + i2 = " + (i1 + i2))
  println("i1 / i2 = " + (i1 / i2))
  println("-i1 = " + (-i1))
  println("i1 * i2 == i3 = " + (i1 * i2 == i3))
  println("i1 < i2 = " + (i1 < i2))
  println("i3.## = " + i3.##)
  
  var r1 = Real(98.6)
  var r2 = Real(-32.0)
  println("-r1 = " + -r1)
  println("r1 * r2 = " + (r1 * r2))
  println("r1 == r2 = " + (r1 == r2))
  println("r1 < r2 = " + (r1 < r2))
  println("r1.## = " + r1.##)
  
  println("r1 * i2 = " + (r1 * i2))
  println("i1 * r2 = " + (i1 * r2))
  
  val t = Boole(true)
  val f = Boole(false)
  println("t && f = " + (t && f))
  println("t || f = " + (t || f))
  println("!t = " + (!t))
  
  val globalEnv = new Environment
  val pi = Identifier("pi")
  val num = Real(3.14)
  globalEnv.put(pi, num)
  println(pi.execute(globalEnv))
  println(num.execute(globalEnv))
}