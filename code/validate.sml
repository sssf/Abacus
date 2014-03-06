(*
use "enviroment.sml";
use "datatypes.sml";
use "stack.sml";
use "functions.sml";
use "lexical_analyzer.sml";
use "input.sml";
*)


(* recursive-descent parser implmenenting the following language:
    A -> Variable = E | E
    E -> N Operator E | N
    N -> T | -T
    T -> Number | Variable | Function N | (E)

  where Open = "(", Closed = ")" and Negate = "-"
*)
local

  fun A ([]) = (false, [])
    | A (Variable(_)::Assignment::tokens) = A(tokens)
    | A (tokens) = E(tokens)

  (* E(tokens)
     TYPE: token list -> (bool * token list)
     PRE: true
     POST: true and remaining tokens if tokens are a valid construct of "E", otherwise false and tokens
     VARIANT: |tokens|
     EXAMPLE: E([Number "1", Operator "+", Number "2", Operator "*", Function "sin", Open, Number "2", Operator "*", Variable "Pi", Close]) = (true, [])
  *)
  and E ([])     = (false, [])
    | E (tokens) =
    let
      val (test, l) = e0(tokens) (* "N Operator E" *)
    in
      if test  then
        (true, l)
      else
        let
          val (test, l) = e1(tokens) (* "N" *)
        in
          if test then
            (true, l)
          else
            (false, tokens)
        end
    end


  (* N(tokens)
     TYPE: token list -> (bool * token list)
     PRE: true
     POST: true and remaining tokens if tokens are a valid construct of "N", otherwise false and tokens
     VARIANT: |tokens|
     EXAMPLE: see function E above
  *)
  and N([]) = (false, [])
    | N(tokens) =
    let
      val (test, l) = n0(tokens) (* T *)
    in
      if test then
        (true, l)
      else
        let
          val (test, l) = n1(tokens) (* -T *)
        in
          if test then
            (true, l)
          else
            (false, tokens)
        end
    end


  (* T(tokens)
     TYPE: token list -> (bool * token list)
     PRE: true
     POST: true and remaining tokens if tokens are valid construct of "T", otherwise false and tokens
     VARIANT: |tokens|
     EXAMPLE: see function E above
  *)
  and T([]) = (false, [])
    | T(tokens) =
    let
      val (test, l) = t0(tokens) (* "number | variable" *)
    in
      if test then
        (true, l)
      else
        let
          val (test, l) = t1(tokens) (* "Function N" *)
        in
          if test then
            (true, l)
          else
            let
              val (test, l) = t2(tokens) (* "(E)" *)
            in
              if test then
                (true, l)
              else
                (false, tokens)
            end
        end
    end



  (* e0(tokens)
     TYPE: token list -> (bool * token list)
     PRE: true
     POST: true and remaining tokens if tokens are a valid construct of "N Operator E", otherwise false and tokens
     VARIANT: |tokens|
  *)
  and e0(tokens) =  (* N operator E *)
    let
      val (test, l) = N(tokens)
    in
      if test then
        let
          val (test, l) = case l of Operator(_)::rest => (true, rest) | tokens => (false, tokens)
        in
          if test then
            let
              val (test, l) = E(l)
            in
              if test then
                (true, l)
              else
                (false, tokens)
            end
          else
            (false, tokens)
        end
      else
        (false, tokens)
    end


  (* e1(tokens)
     TYPE: token list -> (bool * token list)
     PRE: true
     POST: true and remaining tokens if tokens are a valid construct of "N", otherwise false and tokens
     VARIANT: |tokens|
  *)
  and e1(tokens) = N(tokens)   (* N *)


  (* TODO: move this into N *)
  (* n0(tokens)
     TYPE: token list -> (bool * token list)
     PRE: true
     POST: true and remaining tokens if tokens are a valid construct of "T", otherwise false and tokens
     VARIANT: |tokens|
  *)
  and n0(tokens) = T(tokens)        (* T *)


  (* n1(tokens)
     TYPE: token list -> (bool * token list)
     PRE: true
     POST: true and remaining tokens if tokens are a valid construct of "-T", otherwise false and tokens
     VARIANT: |tokens|
     EXAMPLE: TODO
  *)
  and n1(Negate::rest) = T(rest)   (* -T *)
    | n1(tokens)       = (false, tokens)


  (* t0(tokens)
     TYPE: token list -> (bool * token list)
     PRE: true
     POST: true and tail of tokens if head of tokens is a Number,
           true and tail of tokens if head of tokens is a Variable
           otherwise false and tokens
  *)
  and t0 (Number(_)::rest)   = (true, rest)     (* number *)
    | t0 (Variable(_)::rest) = (true, rest)     (* variable *)
    | t0 (tokens)            = (false, tokens)


  (* t1(tokens)
     TYPE: token list -> (bool * token list)
     PRE: true
     POST: true and tail of tokens if head of tokens is a Function, otherwise false and tokens
     VARIANT: |tokens|
  *)
  and t1(Function(_)::rest) = N(rest)           (* "Function N" *)
    | t1(tokens)            = (false, tokens)


    (* TODO: try to clean this up! *)
  (* t2(tokens)
     TYPE: token list -> (bool * token list)
     PRE: true
     POST: true and remaining tokens, if tokens match the construct "(E)", otherwise false and tokens
     VARIANT: |tokens|
  *)
  and t2(Open::rest) = (* match "(" *)
    let

      (* close(tokens)
         TYPE: token list -> (bool * token list)
         PRE: true
         POST: true and tail of tokens if head of tokens is a Closed token, otherwise false and tokens
      *)
      fun close(Closed::rest) = (true, rest)
        | close(tokens)       = (false, tokens)

      val (test, l) = E(rest)       (* match "E" *)
    in
      if test then
        close(l)    (* match ")" *)
      else
        (false, Open::rest)
    end
    | t2(tokens) = (false, tokens);


in
  (* validate(tokens)
     TYPE: token list -> bool
     PRE: true
     POST: true if tokens are a valid construct of "A" and no tokens remaining (see above or documentation ), false otherwise
     VARIANT: |tokens|
     EXAMPLE: validate([Number "1", Operator "+", Number "2", Operator "*", Function "sin", Open, Number "2", Operator "*", Variable "Pi", Close]) = true
  *)
  fun validate(tokens) = A(tokens) = (true, [])

end;




