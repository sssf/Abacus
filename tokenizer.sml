use "datatypes.sml";
use "functions.sml";



fun accToString(acc) = implode(List.rev(acc)); 

(* TODO: use tokens instead of strings *)
(* How do we handle multi character infix operators like mod, div, etc.
 *  how do we handle postfix operators like "!" 
 *)



fun s0([]) = []
  | s0(head::tail) =
    if head = #"0" then
      n2(tail, [head])
    else if Char.isDigit(head) then
      n1(tail, [head])
    else if Char.isAlpha(head) then
      i1(tail, [head])
   else if isOperator(head) then
      head::o1(tail)
    else if Char.isSpace(head) then
      s1(tail,[])
    else
      raise Fail("invalid input: "^Char.toString(head)^" found")


and s1([], acc) = []
  | s1(head::tail, acc) =
    if Char.isSpace(head) then
      s1(tail, acc)
    else
     s0(head::tail)



and i1([], acc) = []
  | i1(head::tail, acc) =
    if Char.isAlphaNum(head) orelse head = #"_" then (* because underlines are beautiful! *)
      i1(tail, head::acc)
    else
      accToString(acc)::s0(head::tail) (* remember to put head back on tail *)



and n4([], acc) = []
  | n4(head::tail, acc) =
    if Char.isDigit(head) then
      n4(tail, head::acc)
    else
      accToString(acc)::s0(head::tail)

and n3([], acc) = []
  | n3(head::tail, acc) =
    if Char.isDigit(head) then
      n4(tail, head::acc)
    else
      raise Fail(". expected but "^(Char.toString(head))^" was found")

and n2([], acc) = []
  | n2(head::tail, acc) =
    if head = #"." then
      n3(tail, head::acc)
    else
      accToString(acc)::s0(head::tail)

and n1([], acc) = []
  | n1(head::tail, acc) =
    if Char.isDigit(head) then
      n1(tail, head::acc)
    else if head = #"." then
      n3(tail, head::acc)
    else
      accToString(acc)::s0(head::tail)

and o1([], acc) = []
  | o1(head::tail, acc) =
    if isOperator(head) then








