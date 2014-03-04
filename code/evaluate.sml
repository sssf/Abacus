fun evaluate([],stack, enviroment)                              = (print("set ans\n"); setVar(enviroment,"ans",top(stack)))
  | evaluate(Assignment::Variable(name)::xs, stack, enviroment) = (print("assign: \n"); evaluate(xs, stack, setVar(enviroment, name, top(stack)))) (* weird! *)
  | evaluate(Number(n)::xs, stack, enviroment)                  = (print("push number\n"); evaluate(xs, push(stack, (valOf(Real.fromString(n))) ),enviroment))
  | evaluate(Variable(name)::xs, stack, enviroment)             = (print("push variable\n"); evaluate(xs, push(stack, getValue(enviroment,name)),enviroment))
  | evaluate(x::xs, stack, enviroment)                          = (print("function\n"); evaluate(xs, getFunction(x) stack,enviroment));
