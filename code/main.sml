(* include files and start Abacus! *)
use "enviroment.sml";
use "datatypes.sml";
use "stack.sml";
use "functions.sml";
use "evaluate.sml";
use "tokenize.sml";
use "translate.sml";
use "input.sml";
use "validate.sml";

local


  (* logo()
     TYPE: unit -> unit
     PRE:  file data/logo.txt must exist
     POST: unit
     SIDE-EFFECTS: prints content of data/logo.txt to stdOut
  *)
  fun logo()    = printFile("data/logo.txt");

  (* welcome()
     TYPE: unit -> unit
     PRE:  file data/welcome.txt must exist
     POST: unit
     SIDE-EFFECTS: prints content of data/welcome.txt to stdOut
  *)
  fun welcome() = printFile("data/welcome.txt");

  (* help()
     TYPE: unit -> unit
     PRE:  file data/help.txt must exist
     POST: unit
     SIDE-EFFECTS: prints content of data/help.txt to stdOut
  *)
  fun help   () = printFile("data/help.txt");

  (* credits()
     TYPE: unit -> unit
     PRE:  file data/credits.txt must exist
     POST: unit
     SIDE-EFFECTS: prints content of data/credits.txt to stdOut
  *)
  fun credits() = (logo(); printFile("data/credits.txt"));


  (* exception Quit
     USE: raise Quit in order to exit main loop
  *)
  exception Quit;


  (* handleInput()
     TYPE: unit -> string
     PRE:  true
     POST: first non-command, non-black line read from stdIn
     SIDE-EFFECTS: raises Quit if quit command is read from stdIn, prints to stdOut
     VARIANT: number of commands in stdIn
  *)
  fun handleInput() =
    let
      val exp = input(" ~ ") (* read input from stdIn *)
    in
      (* handle commands or return input *)
      case exp of "help"    => (help();    handleInput()) |
                  "credits" => (credits(); handleInput()) |
                  "logo"    => (logo();    handleInput()) |
                  "exit"    => raise Quit                 |
                  "quit"    => raise Quit                 |
                  ":q"      => raise Quit                 |
                  ":Q"      => raise Quit                 | _ => exp
    end;


  (* validExpression()
     TYPE: unit -> token list
     PRE:  true
     POST: token list representing a valid expression
     SIDE-EFFECTS: handles Fail exceptions from tokeize and validate, prints to stdOut if invalid expression is found
     VARIANT: number of invalid expressions the user types
  *)
  fun validExpression() =
    let
      val exp = handleInput()
      val tokens = tokenize(exp) handle Fail(message) => (print(" => "^message^"\n"); validExpression())
      val valid = validate(tokens) handle Fail(message) => (print(" => "^message^"\n"); validate(validExpression()))
    in
      if valid then
        tokens
      else
        (print(" => invalid expression\n"); validExpression())
    end;


in


  (* abacus()
     TYPE: unit -> unit
     PRE:  true
     POST: ()
     SIDE-EFFECTS: prints to stdOut, reads from stdIn
  *)
  fun abacus() =
    let
      val _ = TextIO.inputLine(TextIO.stdIn) (* eat "\n" of the input buffer *)
      val _ = (logo(); welcome())


      (* run(env)
         TYPE: Enviroment -> Enviroment
         PRE:  true
         POST: env with user typed expression evaluated on
         SIDE-EFFECTS: prints to stdOut, reads from stdIn
         VARIANT: number of nun-quit commands typed by user
      *)
      fun run(env) =
        let
          val env = evaluate(env, EmptyStack, translate(validExpression()) ) handle Fail(message) => (print(" => "^message^"\n"); run(env))
          val _   = print(" => "^format(env))
        in
          run(env) handle Fail(message) => (print(" => "^message^"\n"); run(env))
        end
      val _ = run(defaultEnviroment) handle Fail(message) => (print(" => "^message^"\n"); run(defaultEnviroment)) | Quit => defaultEnviroment
    in
     ()
    end;
end;
