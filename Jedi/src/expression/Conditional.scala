package expression

import context.Environment
import value._

case class Conditional(condition: Expression, consequent: Expression, alternative: Expression = null) extends SpecialForm {

  override def toString = {
    var result = "if (" + condition + ") " + consequent
    if (alternative != null) result = result + " else " + alternative
    result
  }

  // etc.
  def execute(env: Environment) = {
    if (condition.execute(env).asInstanceOf[Boole].value)
      consequent.execute(env)
    else if (alternative != null)
      alternative.execute(env)
    else Notification.UNSPECIFIED
  }
}