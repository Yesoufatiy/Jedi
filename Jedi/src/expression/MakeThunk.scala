package expression

import context.Environment
import value._

case class MakeThunk(body: Expression) extends SpecialForm {
  def execute(env: Environment) = {
    new Thunk(env)
  }
}