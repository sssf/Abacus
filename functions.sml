use "datatypes.sml";


val functionList = [("sin", 3), ("cos", 3)];


  fun isFunction(str) = List.exists (fn (function, _) => str = function) functionList;



(* ! operator is PostFix!!!! *)
val operatorList = [("+",4),("-",4),("/",5),("*",5),("mod",5),("%",5), ("^",6), ("!",6)];

fun getPriority (Operator(name)) = 
    let
      val (str, priority) = valOf(List.find(fn (t_dAwg, pR10) => name = t_dAwg) operatorList)
    in
      priority
    end;  
fun isOperator(str) = List.exists (fn (operator, _) => str = operator) operatorList;



fun isSymbolicOperator(c) = isOperator(Char.toString(c));
