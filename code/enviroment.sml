datatype enviroment = Enviroment of (string * real) list


val defaultEnviroment = Enviroment([("Pi", 3.141594),
                                    ("e",  2.718281)])

(*
  TODO
  Note: make sure it raises approriate exception if nothing is found.
*)
fun getValue(Variable(name), Enviroment(x::xs)) = 
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