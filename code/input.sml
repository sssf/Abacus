(*
readLine ()
TYPE: unit -> string option
PRE: true
POST: the current line of input from stdIn
SIDE-EFFECTS: advances the stream position of stdIn
*)
fun readLine () =
	(TextIO.inputLine TextIO.stdIn);


(*
str s
TYPE: string option -> string option
PRE: true
POST: NONE if s is "\n" else s
EXAMPLE: str(SOME "a") = SOME "a"
*)
fun str s = if s = SOME "\n" then NONE else s;


fun eat() =
  if Char.isSpace(valOf(TextIO.lookahead(TextIO.stdIn))) then (TextIO.input1(TextIO.stdIn); eat())
  else ();

(* 
input ()
TYPE: unit -> string
PRE: true
POST: string resulted from side-effects
SIDE-EFFECTS: prints to instructions, takes in line of input
*)
fun input () =
	(
	(* readLine();*) (*UGLY!*)
  eat();
	print("Enter Expression: ");
	case str(readLine()) of 
		SOME r => String.substring(r, 0, size r - 1)
		| NONE => (print "No input!\n";input())
	);


