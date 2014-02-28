(* functionList
   TYPE: (string * int) list
   VALUE: list of functions and their priorities
   TODO: add function
*)
val functionList = [("sin", 42, (fn stack => push(pop(stack),Math.sin(top(stack))))), 
                    ("cos", 42,  (fn stack => push(pop(stack),Math.cos(top(stack)))))];


(* isFunction(str)
   TYPE: string -> bool
   PRE:  true
   POST: true if function with name str exist, false otherwise
   EXAMPLE: isFunction("sin")  = true
            isFunction("bark") = false
*)
fun isFunction(str) = List.exists (fn (function,_ , _ ) => str = function) functionList;


(* NOTE: ! operator is postfix!? *)

(* operatorList
   TYPE: (string * int) list
   VALUE: list of Operators and their priorities
   TODO: add functions
*)

(* Failed attempt
  fun operatorFunction (stack, func) = (push(pop(pop(stack), func(top(stack),top(pop(stack))) )));*)

val operatorList = [("+",4,   (fn stack => push(pop(pop(stack)), top(stack) + top(pop(stack)) ))),
                    ("-",4,   (fn stack => push(pop(pop(stack)), top(stack) - top(pop(stack)) ))),
                    ("/",5,   (fn stack => push(pop(pop(stack)), top(stack) / top(pop(stack)) ))),
                    ("*",5,   (fn stack => push(pop(pop(stack)), top(stack) * top(pop(stack)) ))),
                    ("mod",5, (fn stack => push(pop(pop(stack)), Real.fromInt((Real.trunc(top(stack)) mod Real.trunc(top(pop(stack))))) ))),
                    ("%",5,   (fn stack => push(pop(pop(stack)), Real.fromInt((Real.trunc(top(stack)) mod Real.trunc(top(pop(stack))))) )))(*
                    ("^",6,   (fn stack => push(pop(pop(stack)), top(stack) ^ top(pop(stack)) ))), 
                    ("!",6,   (fn stack => push(pop(pop(stack)), top(stack) ! top(pop(stack)) )))*)];


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
      val found = (List.find (fn (operator, _, _) => name = operator) operatorList)
      val (_, priority, _) = valOf(found)
    in
      priority
    end
  | getPriority(Function(name)) =
      let
        val found = (List.find (fn (function, _, _) => name = function) functionList)
        val (_, priority, _) = valOf(found)
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
fun isOperator(str) = List.exists (fn (operator, _, _) => str = operator) operatorList;


(* isSymbolicOperator(c)
   TYPE: char -> bool
   PRE:  true
   POST: true if operator with symbol c exist, false otherwise
   EXAMPLE: isOperator(#"+")  = true
            isOperator(#";") = false
*)
fun isSymbolicOperator(c) = isOperator(Char.toString(c));
