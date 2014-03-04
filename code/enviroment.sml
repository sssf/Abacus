(*
  REPRESENTATION CONVENTION: Enviroment of (String * real) list represents
                             An envoriment with a list of variables in a tuple (name, value).
  REPRESENTATION INVARIANT:
                             >>TODO<< 
                             None? "Constants" should not be changed?
*)
datatype enviroment = Enviroment of (string * real) list

(*
  TYPE: (string * real)
  VALUE: List of default variables and their values
*)
val defaultEnviroment = Enviroment([("Pi", Math.pi),
                                    ("e",  Math.e)])

(*
  getValue (enviroment, var)
  TYPE: enviroment * string
  PRE: none
  POST: returns the value of the variable with name var
  SIDE-EFFECTS: raises Fail if no variable with name var exists in the enviroment.
  EXAMPLE: getValue(defaultEnviroment, "Pi") = 3.141594...
  Note: make sure it raises approriate exception if nothing is found, and handle it correctly
*)
fun getValue(Enviroment(x::xs), name) = 
  let
    val found = (List.find (fn (str, _) => name = str) (x::xs))
    val (_, value) = valOf(found) handle Option => raise Fail "exit is not a variable stupid!"
  in
    value
  end;

(*
  setVar enviroment, var, val
  TYPE: enviroment * string * real
  PRE: true
  POST: returns a new Enviroment with variable var set to value val.
  VARIANT: length of enviroment
  EXAMPLE: setVar(defaultEnviroment, "Pi", 3.14) = Enviroment [("Pi", 3.14), ("e", 2.718281828)]
*)
fun setVar(Enviroment(x::xs),name,value) =
  let
    fun setVar'([]) = [(name,value)]
      | setVar'((n,v)::xs) =
          if n = name then
            (n,value)::xs
          else
            (n,v)::setVar'(xs)
    in
      Enviroment(setVar'(x::xs))
    end;
