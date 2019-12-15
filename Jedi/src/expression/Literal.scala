package expression

import value.Value
import context.Environment

trait Literal extends Expression with Value {
  def execute(env: Environment) = this
}