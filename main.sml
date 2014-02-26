(* include and start Abacus! *)
use "datatypes.sml";
use "functions.sml";
use "stack.sml";
use "lexical_analyzer.sml";
use "postFix.sml";
use "input.sml";



fun run () =
  let
    val i = input()
  in
    if i = "exit" then
      ()
    else
      (toPostFix(tokenize(i)); run())
  end;
  (*
val i  = input(); 
val tl = tokenize("3 + 4 - 123");
val rp = toPostFix(tl);
*)
