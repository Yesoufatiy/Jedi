package expression

import context.Environment
import value.Notification

case class Declaration(id: Identifier, init: Expression) extends SpecialForm {
  override def toString = "def " + id + " = " + init
  // etc.
  def execute(env: Environment) = {
    env.put(id, init.execute(env))
    Notification.OK
  }
}