(* REPRESENTATION CONVENTION: Number(id)    - represents a number token
                              Variable(id)  - represents a variable token
                              Functions(id) - represents a function token
                              Operator(id)  - represents a operator tokenA
                              Open          - represents a open parenthesis
                              Close         - represents a closed parenthesis

   REPRESENTATION INVARIAN: TODO: None? strings must be valid Numbers, functions etc ?
*)
datatype token = Number   of string
               | Variable of string
               | Function of string
               | Operator of string
               | Assignment
               | Negate
               | Open
               | Closed

(*
fun getValue(Number(n))      = valOf(Real.fromString(n))
  | getValue(Variable(name)) = getValue(name)
  | getValue(_) = raise Fail "token does not have a value";
  *)


(* format(list)
   TYPE: token list -> string
   PRE:  true
   POST: token list as string
   EXAMPLE: TODO
*)
fun format(enviroment) = Real.toString(getValue(enviroment,"ans"))^"\n";(*)
  format([])                 = "\n"
  | format(Number(id)::tail)   = id^" "^format(tail)
  | format(Variable(id)::tail) = id^" "^format(tail)
  | format(Function(id)::tail) = id^" "^format(tail)
  | format(Operator(id)::tail) = id^" "^format(tail)
  | format(Assigment::tail)    = "= "^format(tail)
  | format(Negate::tail)       = "~"^format(tail)
  | format(Open::tail)         = "("^format(tail)
  | format(Closed::tail)       = ") "^format(tail);*)
