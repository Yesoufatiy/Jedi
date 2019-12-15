package expression

import context._
import value.Closure

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {

  override def toString = {
    var result = operator.toString + "("
    if (operands != Nil) {
      result = result + operands.head.toString
      for (operand <- operands.tail) {
        result = result + ", " + operand
      }
    }
    result = result + ")"
    result
  }

  // etc.
  def execute(env: Environment) = {
    var ops = operands.map(_.execute(env))
    try {
      if (flags.paramPassing == flags.BY_NAME)
        ops = operands.map(MakeThunk(_).execute(env))
      else if (flags.paramPassing == flags.BY_TEXT)
        ops = operands.map(MakeText(_).execute(env))
      if (!operator.execute(env).isInstanceOf[Closure]) throw new TypeException
      else operator.execute(env).asInstanceOf[Closure].apply(ops, env)
    } catch {
      case e: UndefinedException => alu.execute(operator, ops)
    }
  }
}