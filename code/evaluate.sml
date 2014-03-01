fun evaluate([],stack) = Number(Real.toString(top(stack)))
  | evaluate(Number(n)::xs, stack) =  evaluate(xs, push(stack, (valOf(Real.fromString(n))) ))
  | evaluate(x::xs, stack) = evaluate(xs, getFunction(x) stack)