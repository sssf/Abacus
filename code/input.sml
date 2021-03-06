local
  (*
    readLine ()
    TYPE: unit -> string option
    PRE: true
    POST: the current line of input from stdIn
    SIDE-EFFECTS: advances the stream position of stdIn
  *)
  fun readLine () =
    valOf(TextIO.inputLine TextIO.stdIn);


  (*
    eat ()
    TYPE: unit -> unit
    PRE: true
    POST: unit
    SIDE-EFFECTS: remove leading all whitespace from StdIn
  *)
  fun eat() =
    let
      val next = TextIO.lookahead(TextIO.stdIn)
    in
      if next <> NONE andalso Char.isSpace(valOf(next)) then
        (TextIO.input1(TextIO.stdIn); print(" ~ "); eat())
      else ()
    end;


in
  (*
    input (msg)
    TYPE: string -> string
    PRE: true
    POST: string resulted from side-effects
    SIDE-EFFECTS: prints to msg, takes in line of input
  *)
  fun input(msg)=
    let
      val _ = (print(msg); eat())
      val r = readLine()
    in
      String.substring(r, 0, size r - 1)
    end;


  (*
    printFile(filename)
    TYPE: string -> unit
    PRE: filename must refer to existing file.
    POST: unit
    SIDE-EFFECTS: prints content of file refered to by filename to stdOut
  *)
  fun printFile(filename) =
    let
      val f = TextIO.openIn(filename)
      (*
        printLines()
        TYPE: unit -> unit
        PRE: true
        POST: unit
        SIDE-EFFECTS: reads a line from f and prints it to stdOut
        VARIANT: number of lines in f
      *)
      fun printLines() =
        case TextIO.inputLine(f) of SOME(line) => (print(line); printLines())
                                  | NONE       => ()
    in
      printLines()
    end;

end;
