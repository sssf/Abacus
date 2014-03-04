(* include files and start Abacus! *)
use "enviroment.sml";
use "datatypes.sml";
use "stack.sml";
use "functions.sml";
use "evaluate.sml";
use "lexical_analyzer.sml";
use "postfix.sml";
use "input.sml";






fun logo() = (print( "                                                                               \n");
              print( "       db         88                                                           \n");
              print( "      d88b        88                                                           \n");
              print( "     d8'`8b       88                                                           \n");
              print( "    d8'  `8b      88,dPPYba,   ,adPPYYba,   ,adPPYba,  88       88  ,adPPYba,  \n");
              print( "   d8YaaaaY8b     88P'    \"8a  \"\"     `Y8  a8\"     \"\"  88       88  I8[    \"\"  \n");
              print( "  d8\"\"\"\"\"\"\"\"8b    88       d8  ,adPPPPP88  8b          88       88   `\"Y8ba,   \n");
              print( " d8'        `8b   88b,   ,a8\"  88,    ,88  \"8a,   ,aa  \"8a,   ,a88  aa    ]8I  \n");
              print( "d8'          `8b  8Y\"Ybbd8\"'   `\"8bbdP\"Y8   `\"Ybbd8\"'   `\"YbbdP'Y8  `\"YbbdP\"'  \n");
              print( "                                                                               \n");
              print( "                                                                               \n"));


(* main()
   TYPE: unit -> bool
   PRE: true
   POST: true
   VARIANT: user types "exit"
   EXAMPLE: main() = true
*)
fun main (env) =
  let
    val str = input("Enter expression: ")
    val env = evaluate(toPostfix(tokenize(str)),EmptyStack,env)
  in
    str = "exit" orelse (print(format (env) ); main(env))
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
