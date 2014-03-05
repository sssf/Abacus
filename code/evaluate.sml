(* evaluate env, stack, l
   TYPE: enviroment * stack * token list -> enviroment
   PRE: l is a valid expression in postfix notation.
   POST: env with variable "ans" set to the result of evaluating l (using the stack)
   VARIANT: length of list
   EXAMPLE: evaluate(defaultEnviroment,EmptyStack,[Number("1"), Number("7"),Operator("+")]) = 
                Enviroment [("Pi", 3.141592654), ("e", 2.718281828), ("ans", 8.0)]
*)
fun evaluate(env, stack, [])                             = setVar(env,"ans", top(stack))
  | evaluate(env, stack, Assignment::Variable(name)::xs) = evaluate(setVar(env, name, top(stack)), stack, xs)
  | evaluate(env, stack, Number(n)::xs)                  = evaluate(env, push(stack, (valOf(Real.fromString(n))) ), xs)
  | evaluate(env, stack, Variable(name)::xs)             = evaluate(env, push(stack, getValue(env,name)), xs)
  | evaluate(env, stack, x::xs)                          = evaluate(env, getFunction(x) stack, xs);
