package expression

import context.Environment
import value._

case class Identifier(val name: String) extends Expression {
  override def toString = name
  def execute(env: Environment) = {
    val exe = env(this)
    if (exe.isInstanceOf[Thunk])
      exe.asInstanceOf[Thunk].apply()
    else if (exe.isInstanceOf[Text])
      exe.asInstanceOf[Text].body.execute(env)
    else exe
  }
}