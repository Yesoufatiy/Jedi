package expression

import context._
import value._

case class Iteration(condition: Expression, body: Expression) extends SpecialForm {
  def execute(env: Environment) = {
    if (condition.execute(env).isInstanceOf[Boole]) {
      while(condition.execute(env).asInstanceOf[Boole].value)
        body.execute(env)
      Notification.DONE
    } else throw new TypeException
  }
}