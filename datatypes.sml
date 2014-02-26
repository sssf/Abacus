
(*datatype type  = Number

datatype token = Token(type, string)
*)
datatype token = Number   of string 
               | Variable of string 
               | Function of string 
               | Operator of string 
               | Open
               | Close;







