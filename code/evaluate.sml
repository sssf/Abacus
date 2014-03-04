fun evaluate(env, stack, [])                             = setVar(env,"ans", top(stack))
  | evaluate(env, stack, Assignment::Variable(name)::xs) = evaluate(stack, setVar(env, name, top(stack)), xs)
  | evaluate(env, stack, Number(n)::xs)                  = evaluate(push(stack, (valOf(Real.fromString(n))) ), env, xs)
  | evaluate(env, stack, Variable(name)::xs)             = evaluate(push(stack, getValue(env,name)), env, xs)
  | evaluate(env, stack, x::xs)                          = evaluate(getFunction(x) stack, env, xs);

