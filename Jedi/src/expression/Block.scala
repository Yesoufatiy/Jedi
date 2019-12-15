package expression

import context.Environment
import value.Value

case class Block(expressions: List[Expression]) extends SpecialForm with Expression {
  def execute(env: Environment): Value = {
    val tempEnv = new Environment(env)
    for (exp <- 0 until expressions.length - 1)
      expressions(exp).execute(tempEnv)
    expressions(expressions.length - 1).execute(tempEnv)
  }
}