

fun log(exp, str) = (print(str^"\n"); exp);


local
  fun accToString(acc) = implode(List.rev(acc));

  fun tokenForIdentifier(str) =
    if isOperator(str) then
      Operator(str)
    else if isFunction(str) then
      Function(str)
    else
      Variable(str);


  fun start([]) = []
    | start(head::tail) =
      if head = #"0" then
        n2(tail, [head])
      else if Char.isDigit(head) then
        n1(tail, [head])
      else if Char.isAlpha(head) then
        i1(tail,[head])
      else if isSymbolicOperator(head) then
        Operator(Char.toString(head))::start(tail)
      else if Char.isSpace(head) then
        start(tail)
      else
        case head of #"(" =>  Open::start(tail)
                   | #")" => Close::start(tail)
                   | _    => raise Fail ("invalid input: "^Char.toString(head)^" was found")


  (* numbers *)
  and n1([], acc) = [Number(accToString(acc))]
    | n1(head::tail, acc) =
      if Char.isDigit(head) then
        n1(tail, head::acc)
      else if head = #"." then
        n3(tail, head::acc)
      else
        Number(accToString(acc))::start(head::tail)


  and n2([], acc) = [Number(accToString(acc))]
    | n2(head::tail, acc) =
      if head = #"." then
        n3(tail, head::acc)
      else
        Number(accToString(acc))::start(head::tail)


  and n3([], acc) = raise Fail "0-9 expected but reached end of input"
    | n3(head::tail, acc) =
      if Char.isDigit(head) then
        n4(tail, head::acc)
      else
        raise Fail("0-9 expected but "^(Char.toString(head))^" was found")


  and n4([], acc) = [Number(accToString(acc))]
    | n4(head::tail, acc) =
      if Char.isDigit(head) then
        n4(tail, head::acc)
      else
        Number(accToString(acc))::start(head::tail)


  (* indentifiers *)
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


  and i2([], acc) = [tokenForIdentifier(accToString(acc))]
    | i2(head::tail, acc) =
      if Char.isAlphaNum(head) orelse head = #"_" then
        i2(tail, head::acc)
      else
        tokenForIdentifier(accToString(acc))::start(head::tail)
in
  fun tokenize(str) = start(explode(str))
end;
