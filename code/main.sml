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


  fun logo() = (print( "                                                                               \n");
                print( "       db         88                                                           \n");
                print( "      d88b        88                                                           \n");
                print( "     d8'`8b       88                                                           \n");
                print( "    d8'  `8b      88,dPPYba,   ,adPPYYba,   ,adPPYba,  88       88  ,adPPYba,  \n");
                print( "   d8YaaaaY8b     88P'    \"8a  \"\"     `Y8  a8\"     \"\"  88       88  I8[    \"\"  \n");
                print( "  d8\"\"\"\"\"\"\"\"8b    88       d8  ,adPPPPP88  8b          88       88   `\"Y8ba,   \n");
                print( " d8'        `8b   88b,   ,a8\"  88,    ,88  \"8a,   ,aa  \"8a,   ,a88  aa    ]8I  \n");
                print( "d8'          `8b  8Y\"Ybbd8\"'   `\"8bbdP\"Y8   `\"Ybbd8\"'   `\"YbbdP'Y8  `\"YbbdP\"'  \n");
                print( "                                                                               \n");
                print( "                                                                               \n"));


  fun welcome() = print("Welcome to Abacus! \nType \"help\", \"credits\" for more information. \n");
  fun help   () = printFile("help.txt") 
  fun credits() = print("\n   Developed by Micael Loberg, Tommy Vagbratt and Wenting Jin \n\n");


exception Quit;

  fun handleInput() =
    let
      val exp = input(" ~ ")
    in
      case exp of "help"    => (help();    handleInput()) |
                  "credits" => (credits(); handleInput()) |
                  "logo"    => (logo();    handleInput()) |
                  "exit"    => raise Quit                 |
                  "quit"    => raise Quit                 |
                  ":q"      => raise Quit                 |
                  "env"     => raise Quit                 |
                  ":Q"      => raise Quit                 | _ => exp
    end;

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
  (*
  fun printFile(filename) =
    let
      val f = TextIO.openIn(filename)
      fun printFile() =
        case TextIO.inputLine(f) of SOME(line) => (print(line); printFile())
                                  | NONE       => ()
    in
      printFile()
    end;
    *)
  fun abacus() =
    let
      val _ = TextIO.inputLine(TextIO.stdIn) (* eat "\n" of the input buffer *)
      val _ = (logo(); welcome())
      fun run(env) =
        let
          val env = evaluate(env, EmptyStack, translate(validExpression()) ) handle Fail(message) => (print(" => "^message^"\n"); run(env))
          val _   = print(" => "^format(env))
        in
          run(env) handle Fail(message) => (print(" => "^message^"\n"); run(env))
        end
      val _ = run(defaultEnviroment) handle Fail(message) => (print(" => "^message^"\n"); run(defaultEnviroment)) | Quit => defaultEnviroment
    in
     ()(* run(defaultEnviroment) handle Fail(message) => (print(" => "^message^"\n"); run(defaultEnviroment)) | Quit => defaultEnviroment*)
    end;
end;
