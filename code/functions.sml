(* functionList
   TYPE: (string * int) list
   VALUE: list of functions and their priorities
   TODO: add function
*)
val functionList = [("sin",  42, (fn stack => push(pop(stack),Math.sin(top(stack))))),
                    ("sqrt", 42, (fn stack => push(pop(stack),Math.sqrt(top(stack))))),
                    ("log",  42, (fn stack => push(pop(stack),Math.log10(top(stack))))),
                    ("ln",   42, (fn stack => push(pop(stack),Math.ln(top(stack))))),
                    ("min",  42, (fn stack => push(pop(stack),Real.min(top(pop(stack)),top(stack))))),
                    ("max",  42, (fn stack => push(pop(stack),Real.max(top(pop(stack)),top(stack))))),
                    ("abs",  42, (fn stack => push(pop(stack),Real.abs(top(stack))))),
                    ("cos",  42, (fn stack => push(pop(stack),Math.cos(top(stack)))))];

(* isFunction(str)
   TYPE: string -> bool
   PRE:  true
   POST: true if function with name str exist, false otherwise
   EXAMPLE: isFunction("sin")  = true
            isFunction("bark") = false
*)
fun isFunction(str) = List.exists (fn (function,_ , _ ) => str = function) functionList;


(* operatorFunction f s
   TYPE: ('a * 'a -> 'a) -> 'a stack -> 'a stack
   PRE: true (>>>>>?<<<<<)
   POST: a stack with the two first elements popped off 
         and replaced with the result of function f applied to the two
         popped elements
*)
fun operatorFunction func stack = push(pop(pop(stack)), func(top(stack),top(pop(stack))) );


(* NOTE: ! operator is postfix!?
         How is ! defined for real numbers? 
         ! can raise exceptions*)

(* operatorList
   TYPE: (string * int) list
   VALUE: list of Operators and their priorities
   TODO: add functions
*)
val operatorList = [("+",4,   operatorFunction (fn (x,y) => x + y)),
                    ("-",4,   operatorFunction (fn (x,y) => y - x)),
                    ("/",5,   operatorFunction (fn (x,y) => y / x)),
                    ("*",5,   operatorFunction (fn (x,y) => x * y)),
                    ("mod",5, operatorFunction (fn (x,y) => Real.fromInt(Real.trunc(x) mod Real.trunc(y)))),
                    ("%",5,   operatorFunction (fn (x,y) => Real.fromInt(Real.trunc(x) mod Real.trunc(y)))),
                    ("^",6,   operatorFunction (fn (y,x) => Math.pow(x,y) ))(*, 
                    ("!",6,   operatorFunction (fn (x,y) => Real.fromInt(Real.trunc(x) mod Real.trunc(y)))))*)];


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


(* getFunction token
   TYPE: token -> (real * real -> real)
   PRE:  token exist in operatorList or functionList
   POST: anonymous function of operator or function
   SIDE-EFFECTS: raises Fail exception if token isn't a Function or Operator
   EXAMPLE: getPriority(Operator("+"))   = fn (x,y) => x+y)
            getPriority(Function("sin")) = Math.sin()
*)
fun getFunction (Operator(name)) =
    let
      val found = (List.find (fn (operator, _, _) => name = operator) operatorList)
      val (_, _, function) = valOf(found)
    in
      function
    end
  | getFunction(Function(name)) =
      let
        val found = (List.find (fn (function, _, _) => name = function) functionList)
        val (_, _, f) = valOf(found)
      in
        f
      end
  | getFunction(_) = raise Fail "expected Function or Operator token";


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
