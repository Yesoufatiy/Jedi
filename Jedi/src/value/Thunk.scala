package value

import expression._
import context.Environment

class Thunk(env: Environment) extends Closure(Nil, null, env) {
  var cache: Value = null
  def apply() = {
    if (cache == null) {
      cache = super.apply(Nil, env)
    }
    cache
  }
}