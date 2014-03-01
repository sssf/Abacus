exception EmptyStackException;

datatype 'a stack = EmptyStack | Stack of 'a list

fun pop(EmptyStack) = raise EmptyStackException
  | pop(Stack([x])) = EmptyStack
  | pop(Stack (x :: xs)) = Stack(xs);

fun push(EmptyStack,x)  = Stack (x::[])
  | push(Stack(xs), x)  = Stack (x :: xs);

fun top(EmptyStack) = raise EmptyStackException
  | top(Stack([x])) = x
  | top(Stack (x :: xs)) = x;

fun hasNext(EmptyStack) = false
  | hasNext(Stack([x])) = true
  | hasNext(Stack(x::xs)) = true;
