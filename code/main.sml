(* include files and start Abacus! *)
use "datatypes.sml";
use "enviroment.sml";
use "stack.sml";
use "functions.sml";
use "evaluate.sml";
use "lexical_analyzer.sml";
use "postfix.sml";
use "input.sml";


(* main()
   TYPE: unit -> bool
   PRE: true
   POST: true
   VARIANT: user types "exit"
   EXAMPLE: main() = true
*)
fun main () =
  let
    val str = input("Enter expression: ")
  in
    str = "exit" orelse (print(format ([evaluate(toPostfix(tokenize(str)),EmptyStack)]) ); main())
  end;



(*
fun main () =
  let
    val i = input("Enter expression: ")
  in
    if i = "exit" then
      ()
    else
      (toPostFix(tokenize(i)); main())
  end;
*)
