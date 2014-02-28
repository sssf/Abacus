
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
    fun	toPostFix'([], EmptyStack, q) = (print("Done \n");q) (*Base case*)
      | toPostFix'([], s, q) = (print("Moving operator from s to q \n");toPostFix'([], pop(s), top(s)::q)) (*Input list is empty, but there are still items in the operator stack*)

      | toPostFix'(Number(n)::xs, s, q) = (print("Add number to q \n");toPostFix'(xs,s, Number(n)::q)) (*Input is a Number, moves it to the output queue*)
      | toPostFix'(Open::xs, s, q) = (print("Add ( to s \n");toPostFix'(xs, push(s,Open),q)) (*Input is a left parantheses, adds it to the operator stack*)

      (*Input is right parantheses, move operators from stack to queue untill the matching left parantheses is found*)
      | toPostFix'(Closed::xs, s, q) = (print(") found, check for ( \n");if (top(s) = Open) then (print("    Removed brackets \n");toPostFix'(xs,pop(s),q)) else (print("    ( not found \n");toPostFix'(Closed::xs, pop(s), top(s)::q)))
      | toPostFix'(Closed::xs, EmptyStack, q) = raise Fail "I like turtles"(*Left parantheses not found, raise exception*)

      (*Input is a new expression. Turns expression to postfix, evaluates it and puts it in the output queue*)
      (*| toPostFix'(Expression(x::xs)::ys, s, q) = toPostFix'(ys, s, enqueue(q,evaluate(toPostFix(x::xs))))*)
      | toPostFix'(Function(name)::xs, EmptyStack, q) = toPostFix'(xs, push(EmptyStack, Function(name)),q)
      | toPostFix'( Operator(opr)::xs, EmptyStack, q) = (print("Adding operator to empty stack \n");toPostFix'(xs, push(EmptyStack, Operator(opr)), q))(*Operator added to otherwhise empty stack*)
      | toPostFix'( Operator(opr)::xs, s, q) = (*If operator on stack have higher priority than input, move from stack to queue, else add input to stack*)
        let
          val (prio) = getPriority(Operator(opr))
          val (prio') = if top(s) <> Open then getPriority(top(s)) else 0
        in
          (print("Checking priority \n");
          if prio' > prio then
            (print("    operator on stack is higher than input \n");toPostFix'( Operator(opr)::xs, pop(s), top(s)::q))
          else
            (print("    Input have higher priority \n");toPostFix'(xs, push(s, Operator(opr)), q)))
        end
  in
    List.rev(toPostFix'(l,EmptyStack,[]))
  end;


(*evaluate(queueToList(toPostFix'(formatInput(explode(str)),emptyStack,emptyQueue)),emptyStack);*)
