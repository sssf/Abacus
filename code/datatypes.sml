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
               | Open
               | Closed





(* format(list)
   TYPE: token list -> string
   PRE:  true
   POST: token list as string
   EXAMPLE: TODO
*)
fun format([])                 = "\n"
  | format(Number(id)::tail)   = " "^id^format(tail)
  | format(Variable(id)::tail) = " "^id^format(tail)
  | format(Function(id)::tail) = " "^id^format(tail)
  | format(Operator(id)::tail) = " "^id^format(tail)
  | format(Open::tail)         = " ("^format(tail)
  | format(Closed::tail)       = " )"^format(tail)
  | format(_) = raise Fail "FAIL";
