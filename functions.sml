


val functionList = ["sin", "cos"];

fun isFunction(str) = List.exists (fn function => str = function) functionList



(* ! operator is PostFix!!!! *)
val operatorList = ["+","-","/","*","mod","%", "^", "!"];

fun isOperator(str) = List.exists (fn operator => str = operator) operatorList;

