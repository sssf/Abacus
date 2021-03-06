(* translate(l)
   TYPE: token list -> token list
   PRE:  l is a valid expression
   POST: elements of l rearranged from infix to postfix notation.
   VARIANT: length of l
   EXAMPLE: translate([Number("1"),Operator("+"),Number("2")]) = [Number("1"),Number("2"),Operator("+")]
*)
(*translate(Variable(name)::Assignment::tokens) = translate(tokens) @ [Assignment, Variable(name)]*)
fun translate(l) =
  let
    fun translate'([], EmptyStack, q) = q (*Base case*)
      | translate'([], s, q) = translate'([], pop(s), top(s)::q) (*Input list is empty, but there are still items in the operator stack*)
      
        (*Matches for a variable followed by the Assignment operator -> concats the variable and assignment to the recursive call*)
      | translate'(Variable(name)::Assignment::xs, s, q) = (Variable(name)::Assignment::(translate'(xs, s, q)))

      | translate'(Number(n)::xs, s, q) = translate'(xs,s, Number(n)::q) (*Input is a Number, moves it to the output queue*)
      | translate'(Variable(n)::xs, s, q) = translate'(xs,s, Variable(n)::q) (*Input is a Number, moves it to the output queue*)
      | translate'(Open::xs, s, q) = translate'(xs, push(s,Open),q) (*Input is a left parantheses, adds it to the operator stack*)

      | translate'(Negate::xs, s, q) = translate'(Function("negate")::xs, s, q) (*Input is a left parantheses, adds it to the operator stack*)

      (*Input is right parantheses, move operators from stack to queue untill the matching left parantheses is found*)
      | translate'(Close::xs, s, q) = if (top(s) = Open) then translate'(xs,pop(s),q) else translate'(Close::xs, pop(s), top(s)::q)
      | translate'( head::xs, EmptyStack, q) = translate'(xs, push(EmptyStack, head), q)(*Operator/function added to otherwhise empty stack*)
      | translate'( Operator(name)::xs, s, q) = (*If operator on stack have higher priority than input, move from stack to queue, else add input to stack*)
        let
          val (prio) = getPriority(Operator(name))
          val (prio') = if top(s) <> Open then getPriority(top(s)) else 0
        in
          if prio' >= prio then
          translate'( Operator(name)::xs, pop(s), top(s)::q)
          else
          translate'(xs, push(s, Operator(name)), q)
        end
        | translate'( Function(name)::xs, s, q) = (*If operator on stack have higher priority than input, move from stack to queue, else add input to stack*)
        let
          val (prio) = getPriority(Function(name))
          val (prio') = if top(s) <> Open then getPriority(top(s)) else 0
        in
          if prio' > prio then
          translate'( Function(name)::xs, pop(s), top(s)::q)
          else
          translate'(xs, push(s, Function(name)), q)
        end
        | translate'(_, _, _) = raise Fail "Invalid input, can't translate" (*Probably cant happen*)
  in
    List.rev(translate'(l,EmptyStack,[]))(*Once done, reverse the list so that evaluate can handle it*)
  end;
