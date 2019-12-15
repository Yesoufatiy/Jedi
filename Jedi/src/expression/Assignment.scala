package expression

import context._
import value._

case class Assignment(id: Identifier, exp: Expression) extends SpecialForm {
  def execute(env: Environment) = {
    val assign = id.execute(env)
    if (assign.isInstanceOf[Variable]) {
      assign.asInstanceOf[Variable].value = exp.execute(env)
      Notification.DONE
    } else throw new TypeException
  }
}