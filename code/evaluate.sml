fun evaluate([],stack, enviroment) = setVar(enviroment,"ans",top(stack))
  | evaluate(Number(n)::xs, stack, enviroment) =  evaluate(xs, push(stack, (valOf(Real.fromString(n))) ),enviroment)
  | evaluate(Variable(name)::xs, stack, enviroment) = evaluate(xs, push(stack, getValue(enviroment,name)),enviroment)
  | evaluate(x::xs, stack, enviroment) = evaluate(xs, getFunction(x) stack,enviroment);
