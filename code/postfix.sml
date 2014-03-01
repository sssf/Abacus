(* toPostfix(l)
   TYPE: token list -> token list
   PRE:  l is a valid expression
   POST: elements of l rearranged from infix to postfix notation.
   VARIANT: length of l
   EXAMPLE: toPostfix([Number("1"),Operator("+"),Number("2")]) = [Number("1"),Number("2"),Operator("+")]
*)
fun toPostfix(l) =
  let
    fun	toPostfix'([], EmptyStack, q) = q (*Base case*)
      | toPostfix'([], s, q) = toPostfix'([], pop(s), top(s)::q) (*Input list is empty, but there are still items in the operator stack*)
      | toPostfix'(Number(n)::xs, s, q) = toPostfix'(xs,s, Number(n)::q) (*Input is a Number, moves it to the output queue*)
      | toPostfix'(Variable(n)::xs, s, q) = toPostfix'(xs,s, Variable(n)::q) (*Input is a Number, moves it to the output queue*)
      | toPostfix'(Open::xs, s, q) = toPostfix'(xs, push(s,Open),q) (*Input is a left parantheses, adds it to the operator stack*)
      (*Input is right parantheses, move operators from stack to queue untill the matching left parantheses is found*)
      | toPostfix'(Closed::xs, s, q) = if (top(s) = Open) then toPostfix'(xs,pop(s),q) else toPostfix'(Closed::xs, pop(s), top(s)::q)
      | toPostfix'( head::xs, EmptyStack, q) = toPostfix'(xs, push(EmptyStack, head), q)(*Operator/function added to otherwhise empty stack*)
      | toPostfix'( head::xs, s, q) = (*If operator on stack have higher priority than input, move from stack to queue, else add input to stack*)
        let
          val (prio) = getPriority(head)
          val (prio') = if top(s) <> Open then getPriority(top(s)) else 0
        in
          if prio' >= prio then
          toPostfix'( head::xs, pop(s), top(s)::q)
          else
          toPostfix'(xs, push(s, head), q)
        end
  in
    List.rev(toPostfix'(l,EmptyStack,[]))
  end;
