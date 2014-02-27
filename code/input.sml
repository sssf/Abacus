(*
readLine ()
TYPE: unit -> string option
PRE: true
POST: the current line of input from stdIn
SIDE-EFFECTS: advances the stream position of stdIn
*)
fun readLine () =
	valOf(TextIO.inputLine TextIO.stdIn);




fun eat() =
    let val next = TextIO.lookahead(TextIO.stdIn) in
	if next <> NONE andalso Char.isSpace(valOf(next)) then (TextIO.input1(TextIO.stdIn); eat()) else () end;

(* 
input (msg)
TYPE: unit -> string
PRE: true
POST: string resulted from side-effects
SIDE-EFFECTS: prints to instructions, takes in line of input
*)

fun input(msg)= 
    let
	val _ = (print(msg); eat())
	val r = readLine()
    in
	String.substring(r, 0, size r - 1)
    end;
