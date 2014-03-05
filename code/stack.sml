(* REPRESENTATION CONVENTION: EmptyStack is a stack with no elements
                              Stack('a list) is a stack of elements with type 'a

   REPRESENTATION INVARIAN: TODO: None?
*)
exception EmptyStackException;

datatype 'a stack = EmptyStack | Stack of 'a list

fun pop(EmptyStack) = raise EmptyStackException
  | pop(Stack([]))  = raise EmptyStackException 
  | pop(Stack([x])) = EmptyStack
  | pop(Stack (x :: xs)) = Stack(xs);

fun push(EmptyStack,x) = Stack(x::[])
  | push(Stack([]),x) = Stack(x::[])
  | push(Stack(xs), x) = Stack(x :: xs);

fun top(EmptyStack) = raise EmptyStackException
  | top(Stack([]))  = raise EmptyStackException
  | top(Stack([x])) = x
  | top(Stack (x :: xs)) = x;

fun hasNext(EmptyStack) = false
  | hasNext(Stack([]))  = false
  | hasNext(Stack([x])) = true
  | hasNext(Stack(x::xs)) = true;
