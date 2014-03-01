
(* test
   TYPE: token list
   VALUE: infix test expresion
*)
val test = [Number("3"), Operator("+"), Number("4"), Operator("*"), Number("2"), Operator("/"),
            Open, Number("1"), Operator("-"), Number("5"), Closed, Operator("^"), Number("2"), Operator("^"), Number("3")];

(* toPostfix(l)
   TYPE: token list -> token list
   PRE:  TODO
   POST: TODO
   VARIANT: TODO
   EXAMPLE: TODO
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
      (*| toPostFix'(Closed::xs, EmptyStack, q) = raise Fail "I like turtles"Left parantheses not found, raise exception*)

      (*Input is a new expression. Turns expression to postfix, evaluates it and puts it in the output queue*)
      (*| toPostFix'(Expression(x::xs)::ys, s, q) = toPostFix'(ys, s, enqueue(q,evaluate(toPostFix(x::xs))))*)
      (*| toPostFix'(Function(name)::xs, EmptyStack, q) = toPostFix'(xs, push(EmptyStack, Function(name)),q)*)
      | toPostFix'( head::xs, EmptyStack, q) = toPostFix'(xs, push(EmptyStack, head), q)(*Operator added to otherwhise empty stack*)
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


(*evaluate(queueToList(toPostFix'(formatInput(explode(str)),emptyStack,emptyQueue)),emptyStack);*)
