fun evaluate(env, stack, [])                             = setVar(env,"ans", top(stack))
  | evaluate(env, stack, Assignment::Variable(name)::xs) = evaluate(setVar(env, name, top(stack)), stack, xs)
  | evaluate(env, stack, Number(n)::xs)                  = evaluate(env, push(stack, (valOf(Real.fromString(n))) ), xs)
  | evaluate(env, stack, Variable(name)::xs)             = evaluate(env, push(stack, getValue(env,name)), xs)
  | evaluate(env, stack, x::xs)                          = evaluate(env, getFunction(x) stack, xs);

