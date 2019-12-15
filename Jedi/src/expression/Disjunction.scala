package expression

import context.Environment
import value._

case class Disjunction(operands: List[Expression]) extends SpecialForm {

  override def toString = {
    var result = if (operands == Nil) "" else operands.head.toString
    for (operand <- operands.tail) result = result + " || " + operand.toString()
    result
  }

  // etc.
  def execute(env: Environment) = {
    def helper(ops: List[Expression]): Boole = {
      if (ops == Nil)
        Boole(false)
      else if (ops.head.execute(env).asInstanceOf[Boole].value)
        Boole(true)
      else
        helper(ops.tail)
    }
    helper(operands)
  }
}