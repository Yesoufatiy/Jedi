package expression

import value.Value
import context.Environment

trait Expression {
  def execute(env: Environment): Value
}