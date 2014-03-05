local

(* accToString(acc)
     TYPE: char list -> string
     PRE:  true
     POST: acc reversed and converted to string
     EXAMPLE: accToString([#"s", #"n", #"a"]) = "ans"
  *)
  fun accToString(acc) = implode(List.rev(acc));

  (* tokenForIdentifier(str)
     TYPE: string -> token
     PRE:  true
     POST: Operator(str) if str is operator
           Function(str) if str is function
           Variable(str) otherwise
     EXAMPLE: tokenForIdentifier("sin") = Function("sin")
              tokenForIdentifier("mod") = Operator("mod")
  *)
  fun tokenForIdentifier(str) =
    if isOperator(str) then
      Operator(str)
    else if isFunction(str) then
      Function(str)
    else
      Variable(str);

  (* fixNegation(tokens)
     TYPE: token list -> token list
     PRE:  true
     POST: tokens with negation corrected
     VARIANT: |tokens|
     EXAMPLE: fixNegation([Number("1"), Operator("+"), Operator("-"), Variable("x")]) = [Number("1"), Operator("+"), Negate, Variable("x")]
  *)
  fun fixNegation(Operator("-")::rest) = fixNegation(Negate::rest)
    | fixNegation(tokens) =
    let
      fun fixNegation'([]) = []
        | fixNegation'([token]) = [token]
        | fixNegation'(Operator(x)::Operator("-")::rest) = Operator(x)::fixNegation'(Negate::rest)
        | fixNegation'(Negate::Operator("-")::rest) = Negate::fixNegation'(Negate::rest) (* micael really wanted support for --...-x *)
        | fixNegation'(Function(x)::Operator("-")::rest) = Function(x)::fixNegation'(Negate::rest)
        | fixNegation'(Open::Operator("-")::rest) = Open::fixNegation'(Negate::rest)
        | fixNegation'(head::tail) = head::fixNegation'(tail);
    in
      fixNegation'(tokens)
    end;

  (* start(charList)
     TYPE: char list -> token list
     PRE:  true
     POST: charList translated into tokens according to specification
     SIDE-EFFECTS: raises Fail exeption if charList contain invalid characters. (see specification) TODO: Peter will get back about this.
     EXAMPLE: start(
  *)
  fun start([]) = []
    | start(head::tail) =
      if head = #"0" then (* numbers beginning with 0 *)
        n2(tail, [head])
      else if Char.isDigit(head) then (* numbers beginning with 1-9 *)
        n1(tail, [head])
      else if Char.isAlpha(head) then  (* identifiers, functions, and non-symbolic operators *)
        i1(tail,[head])
      else if isSymbolicOperator(head) then (* handle symbolic operator *)
        Operator(Char.toString(head))::start(tail)
      else if Char.isSpace(head) orelse head = #";" then (* ignore whitespace *)
        start(tail)
      else
        case head of #"=" => Assignment::start(tail) (* "=" *)
                   | #")" =>    Close::start(tail)
                   | #"(" =>      Open::start(tail)
                   | _    => raise Fail ("found invalid character: \""^Char.toString(head)^"\"")


  (* numbers *)
  (* n1(charList, acc)
     TYPE: char list * char list -> token list
     PRE: true
     POST: charList converted to tokens, but first token may be invalid
     EXAMPLE: see function start
  *)
  and n1([], acc) = [Number(accToString(acc))]
    | n1(head::tail, acc) =
      if Char.isDigit(head) then
        n1(tail, head::acc)
      else if head = #"." then
        n3(tail, head::acc)
      else
        Number(accToString(acc))::start(head::tail)


  (* n2(charList, acc)
     TYPE: char list * char list -> token list
     PRE:  true
     POST: charList converted to tokens, but first token:
     EXAMPLE: TODO
  *)
  and n2([], acc) = [Number(accToString(acc))]
    | n2(head::tail, acc) =
      if head = #"." then
        n3(tail, head::acc)
      else
        Number(accToString(acc))::start(head::tail)


  (* n3(charList, acc)
     TYPE: char list * char list -> token list
     PRE:  TODO
     POST: TODO
     EXAMPLE: TODO
  *)
  and n3([], acc) = raise Fail "digit expected, but nothing was found"
    | n3(head::tail, acc) =
      if Char.isDigit(head) then
        n4(tail, head::acc)
      else
        raise Fail("digit expected, but \""^(Char.toString(head))^"\" was found")


  (* n4(charList, acc)
     TYPE: char list * char list -> token list
     PRE:  TODO
     POST: TODO
     EXAMPLE: TODO
  *)
  and n4([], acc) = [Number(accToString(acc))]
    | n4(head::tail, acc) =
      if Char.isDigit(head) then
        n4(tail, head::acc)
      else
        Number(accToString(acc))::start(head::tail)


  (* indentifiers *)
  (* i1(charList, acc)
     TYPE: char list * char list -> token list
     PRE:  TODO
     POST: TODO
     EXAMPLE: TODO
  *)
  and i1([], acc) = [tokenForIdentifier(accToString(acc))]
    | i1(head::tail, acc) =
      if Char.isAlpha(head) orelse head = #"_" then
        i1(tail, head::acc)
      else if Char.isDigit(head) then
        if isOperator(accToString(acc)) then
          Operator(accToString(acc))::start(head::tail)
        else
          i2(tail, head::acc)
      else
        tokenForIdentifier(accToString(acc))::start(head::tail)


  (* i2(charList, acc)
     TYPE: char list * char list -> token list
     PRE:  TODO
     POST: TODO
     EXAMPLE: TODO
  *)
  and i2([], acc) = [tokenForIdentifier(accToString(acc))]
    | i2(head::tail, acc) =
      if Char.isAlphaNum(head) orelse head = #"_" then
        i2(tail, head::acc)
      else
        tokenForIdentifier(accToString(acc))::start(head::tail)


in
  (* tokenize(str)
     TYPE: string -> token list
     PRE:  true
     POST: list of tokens in str
     EXAMPLE: tokenize("2 * sin(x)") = [Number("2"), Operator("*"), Function("sin"), Open, Variable("x"), Close]
  *)
  fun tokenize(str) = fixNegation(start(explode(str)))

end;
