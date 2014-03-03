datatype enviroment = Enviroment of (string * real) list


val defaultEnviroment = Enviroment([("Pi", Math.pi),
                                    ("e",  Math.e)])

(*
  TODO
  Note: make sure it raises approriate exception if nothing is found.
*)
fun getValue(Enviroment(x::xs), name) = 
  let
    val found = (List.find (fn (str, _) => name = str) (x::xs))
    val (_, value) = valOf(found) 
  in
    value
  end;

fun addVariable(Enviroment(x::xs), name, value) = Enviroment((name, value)::x::xs);

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