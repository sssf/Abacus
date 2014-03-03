use "datatypes.sml";
use "stack.sml";
use "functions.sml";
use "lexical_analyzer.sml";
use "input.sml";


local

  fun str([]) = ""
    | str(head::tail) = "x,"^str(tail);


  fun input(s) = "\n";



  fun E([])   = (print("E: false\n"); (false, []))
    | E(list) =
    let
      val _ = print ("E: "^str(list)^input("> "))
      val _ = print("E: test0\n")
      val (test, l) = e0(list)
    in
      if test(* andalso l = []*) then
        (true, l)
      else
        let
          val _ = print("E: test1\n")
          val (test, l) = e1(list)
        in
          if test(* andalso l = []*) then
            (true, l)
          else
            (false, list)
        end
    end
    

  and N([]) = (print("N: false\n");(false, []))
    | N(list) = 
    let
      val _ = print ("N: "^str(list)^input(" $:"))
      val _ = print("N: test0\n")
      val (test, l) = n0(list)
    in
      if test then
        (true, l)
      else
        let
          val _ = print("N: test1\n")
          val (test, l) = n1(list)
        in
          if test then
            (true, l)
          else
            (false, list)
        end
    end


  and T([]) = (print("T: false\n"); (false, []))
    | T(list) =
    let
      val _ = print ("T: "^str(list)^input("> "))
      val _ = print("T: test0\n")
      val (test, l) = t0(list)
    in
      if test then
        (true, l)
      else
        let
          val _ = print("T: test1\n")
          val (test, l) = t1(list)
        in
          if test then
            (true, l)
          else
            let
              val _ = print("T: test2\n")
              val (test, l) = t2(list)
            in
              if test then
                (true, l)
              else
                (false, list)
            end
        end
    end



  and t0 (Number(_)::tail) = (print("num: true\n"); (true, tail))
    | t0 (Variable(_)::tail) = (print("var: true\n"); (true, tail))
    | t0 (list)            = (print("num: false\n"); (false, list))

  and t1(Function(_)::tail) = (print("fun: \n"); N(tail))
    | t1(list)              = (print("fun: false\n"); (false, list))

  and t2(Open::tail) =
    let
      val _ = print("t2: open\n")
      fun close(Closed::tail) = (true, tail)
        | close(list)         = (false, list)
      val test = E(tail)
    in
      if #1(test) then
        close(#2(test))
      else
        (false, Open::tail)
    end
    | t2(list) = (false, list)

  and n0(list) = T(list)

  and n1(Negate::tail) = (print("negate: \n"); T(tail))
    | n1(list)         = (print("negate: false \n"); (false, list))


  and e0(list) = 
    let
      val _ = print ("e0: test0 \n")
      val (test, l) = N(list)
    in
      if test then
        let
          val _ = print("e0: test1\n")
          val (test, l) = case l of Operator(_)::tail => (true, tail) | list => (false, list)
        in
          if test then
            let
              val _ = print("e0: test2\n")
              val (test, l) = E(l)
            in
              if test then
                (true, l)
              else
                (false, list)
            end
          else
            (false, list)
        end
      else
        (false, list)
    end
        

  and e1(list) = (print("e1: \n"); N(list));

in
  fun validate(tokens) =
    let
      val (test, l) = E(tokens)
    in
      test andalso l = []
    end
end;


fun valid(exp) = (print("===========================================================\n"); print(exp); validate(tokenize(exp)));


fun test() = (valid("1") = true andalso
              valid("-1") = true andalso
              valid("1 + 1") = true andalso
              valid("1 + -1") = true andalso
              valid("-1 + 1") = true andalso
              valid("sin 3") = true andalso
              valid("sin x") = true andalso
              valid("sin (x*2)") = true andalso
              valid("sin cos x") = true andalso

              valid("++1") = false andalso
              valid("1++") = false andalso
              valid("1*+1") = false andalso
              valid("1 sin") = false andalso
              valid("* sin 1") = false andalso
              valid("00") = false andalso
              valid("-1 * sin sin sin sin sin sin") = false);





