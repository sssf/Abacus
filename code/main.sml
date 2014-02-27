(* include and start Abacus! *)
use "datatypes.sml";
use "functions.sml";
use "stack.sml";
use "lexical_analyzer.sml";
use "postFix.sml";
use "input.sml";



fun main () =
  let
    val i = input("Enter expression: ")
  in
    if i = "exit" then
      ()
    else
      (toPostFix(tokenize(i)); main())
  end;
  (*
val i  = input(); 
val tl = tokenize("3 + 4 - 123");
val rp = toPostFix(tl);
*)
