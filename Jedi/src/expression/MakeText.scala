package expression

import value.Text
import context.Environment

case class MakeText(body: Expression) extends SpecialForm {
  def execute(env: Environment) = {
    new Text(body)
  }
}