package value

import context._
import expression.Expression
import expression.Identifier

case class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value {
  def apply(args: List[Value], env: Environment): Value = {
    if (params.length != args.length) throw new TypeException
    val tempEnv =
      if (flags.staticScope) new Environment(defEnv)
      else new Environment(env)
    tempEnv.bulkPut(params, args)
    body.execute(tempEnv)
  }
}
