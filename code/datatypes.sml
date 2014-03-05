(* REPRESENTATION CONVENTION: Number(id)    - represents a number token
                              Variable(id)  - represents a variable token
                              Functions(id) - represents a function token
                              Operator(id)  - represents a operator token
                              Assignment    - represents the assignment token
                              Open          - represents a open parenthesis
                              Close         - represents a closed parenthesis

   REPRESENTATION INVARIAN: TODO: None? strings must be valid Numbers, functions etc ?
*)
datatype token = Number   of string
               | Variable of string
               | Function of string
               | Operator of string
               | Assignment
               | Negate
               | Open
               | Close


fun format(enviroment) = Real.toString(getValue(enviroment,"ans"))^"\n";
