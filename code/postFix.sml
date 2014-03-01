(* toPostfix(l)
   TYPE: token list -> token list
   PRE:  l is a valid expression
   POST: elements of l rearranged from infix to postfix notation.
   VARIANT: length of l
   EXAMPLE: toPostFix([Number("1"),Operator("+"),Number("2")]) = [Number("1"),Number("2"),Operator("+")]
*)
fun toPostFix(l) =
  let
    fun	toPostFix'([], EmptyStack, q) = q (*Base case*)
      | toPostFix'([], s, q) = toPostFix'([], pop(s), top(s)::q) (*Input list is empty, but there are still items in the operator stack*)
      | toPostFix'(Number(n)::xs, s, q) = toPostFix'(xs,s, Number(n)::q) (*Input is a Number, moves it to the output queue*)
      | toPostFix'(Variable(n)::xs, s, q) = toPostFix'(xs,s, Variable(n)::q) (*Input is a Number, moves it to the output queue*)
      | toPostFix'(Open::xs, s, q) = toPostFix'(xs, push(s,Open),q) (*Input is a left parantheses, adds it to the operator stack*)
      (*Input is right parantheses, move operators from stack to queue untill the matching left parantheses is found*)
      | toPostFix'(Closed::xs, s, q) = if (top(s) = Open) then toPostFix'(xs,pop(s),q) else toPostFix'(Closed::xs, pop(s), top(s)::q)
      | toPostFix'( head::xs, EmptyStack, q) = toPostFix'(xs, push(EmptyStack, head), q)(*Operator/function added to otherwhise empty stack*)
      | toPostFix'( head::xs, s, q) = (*If operator on stack have higher priority than input, move from stack to queue, else add input to stack*)
        let
          val (prio) = getPriority(head)
          val (prio') = if top(s) <> Open then getPriority(top(s)) else 0
        in
          if prio' >= prio then
          toPostFix'( head::xs, pop(s), top(s)::q)
          else
          toPostFix'(xs, push(s, head), q)
        end
  in
    List.rev(toPostFix'(l,EmptyStack,[]))
  end;