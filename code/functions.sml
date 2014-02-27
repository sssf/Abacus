(* functionList
   TYPE: (string * int) list
   VALUE: list of functions and their priorities
   TODO: add function
*)
val functionList = [("sin", 3), ("cos", 3)];


(* isFunction(str)
   TYPE: string -> bool
   PRE:  true
   POST: true if function with name str exist, false otherwise
   EXAMPLE: isFunction("sin")  = true
            isFunction("bark") = false
*)
fun isFunction(str) = List.exists (fn (function, _) => str = function) functionList;


(* NOTE: ! operator is postfix!? *)

(* operatorList
   TYPE: (string * int) list
   VALUE: list of Operators and their priorities
   TODO: add functions
*)
val operatorList = [("+",4),("-",4),("/",5),("*",5),("mod",5),("%",5), ("^",6), ("!",6)];


(* getPriority(token)
   TYPE: token -> int
   PRE:  token exist in operatorList or functionList
   POST: priority of operator op
   SIDE-EFFECTS: raises Fail exception if token isn't a Function or Operator
   EXAMPLE: getPriority(Operator("+"))   = 4
            getPriority(Function("sin")) = 3
*)
fun getPriority (Operator(name)) =
    let
      val found = (List.find (fn (operator, _) => name = operator) operatorList)
      val (_, priority) = valOf(found)
    in
      priority
    end
  | getPriority(Function(name)) =
      let
        val found = (List.find (fn (function, _) => name = function) functionList)
        val (_, priority) = valOf(found)
      in
        priority
      end
  | getPriority(_) = raise Fail "expected Function or Operator token";


(* isOperator(str)
   TYPE: string -> bool
   PRE:  true
   POST: true if operator with name str exist, false otherwise
   EXAMPLE: isOperator("mod")  = true
            isOperator("bacon") = false
*)
fun isOperator(str) = List.exists (fn (operator, _) => str = operator) operatorList;


(* isSymbolicOperator(c)
   TYPE: char -> bool
   PRE:  true
   POST: true if operator with symbol c exist, false otherwise
   EXAMPLE: isOperator(#"+")  = true
            isOperator(#";") = false
*)
fun isSymbolicOperator(c) = isOperator(Char.toString(c));
