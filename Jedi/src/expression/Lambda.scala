package expression

import context.Environment
import value.Closure

case class Lambda(params: List[Identifier], body: Expression) extends SpecialForm {
  def execute(env: Environment) = {
    Closure(params, body, env)
  }
}