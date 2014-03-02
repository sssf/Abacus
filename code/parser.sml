use "datatypes.sml";


fun E(list) =
  let
    val _ = print "E\n"
    val (test, l) = e0(list)
  in
    if test then
      (true, l)
    else
      let
        val (test, l) = e1(list)
      in
        if test then
          (true, l)
        else
          (false, l)
      end
  end
  

and N(list) = 
  let
    val _ = print "N\n"
    val (test, l) = n0(list)
  in
    if test then
      (true, l)
    else
      let
        val (test, l) = n1(list)
      in
        if test then
          (true, l)
        else
          (false, l)
      end
  end


and T(list) =
  let
    val _ = print "T\n"
    val (test, l) = t0(list)
  in
    if test then
      (true, l)
    else
      let
        val (test, l) = t1(list)
      in
        if test then
          (true, l)
        else
          (false, l)
      end
  end



and t0 (Number(_)::tail) = (true, tail)
  | t0 (list)            = (false, list)

and t1(Function(_)::tail) = N(tail)
  | t1(list)              = (false, list)

and t2(Open::tail) =
  let
    fun close(Closed::tail) = (true, tail)
      | close(list)         = (false, list)
    val test = E(tail)
  in
    if #1(test) then
      close(#2(test))
    else
      (false, #2(test))
  end
  | t2(list) = (false, list)

and n0(list) = T(list)

and n1(Negate::tail) = T(tail)
  | n1(list)         = (false, list)


and e0(list) = 
  let 
    val (test0, l) = N(list)
    val (test1, l) = case l of Operator(_)::tail => (true, tail) | list => (false, list)
    val (test2, l) = E(l)
  in
    ((test0 andalso test1 andalso test2), l)
  end

and e1(list) = N(list);




