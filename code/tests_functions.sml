use "enviroment.sml";
use "datatypes.sml";
use "stack.sml";
use "functions.sml";


  (*Template for writing test cases. Based on assignment3_tests.sml by Dave Clarke


To run these training cases:
   1) launch PolyML shell [poly]
   2) load training set [use "template_tests.sml"]
   3) run tests [test ()]
*)


(* test ()
   TYPE: unit -> unit
   PRE:  true
   POST: ()
   SIDE-EFFECTS: Prints a report, stating whether each test case performed as
                 expected.
 *)

fun test () =
    let

      val testStack = push(push(EmptyStack,5.0),7.0)

      fun floatEqual(a,b) =
        let
          val diff = a - b
        in
          abs(diff) < 0.000001
        end

      (* result (n, f)
         TYPE: int * (int -> bool) -> unit
         PRE:  f is well-defined (i.e., terminates without error) for the call f n
         POST: ()
         SIDE-EFFECTS: Prints a report stating whether test n was successful or not
                       (where f n = true iff the test was successful)
      *)
      fun result (n, f) =
          print ("Test #" ^ Int.toString n ^
           ((if f n then " successful." else " FAILED!")
            handle _ => " raised an (unwanted) exception!") ^ "\n");
  (* test n
       TYPE: int -> bool
       PRE:  1<=n<=10
       POST: true iff test n executes correctly
   *)
  fun test 0 = floatEqual(top(getFunction(Function("sin")) testStack),    0.6569865987)
    | test 1 = floatEqual(top(getFunction(Function("cos")) testStack),    0.7539022543)
    | test 2 = floatEqual(top(getFunction(Function("tan")) testStack),    0.8714479827)           
    | test 3 = floatEqual(top(getFunction(Function("sqrt")) testStack),   2.645751311)
    | test 4 = floatEqual(top(getFunction(Function("log")) testStack),    0.84509804)
    | test 5 = floatEqual(top(getFunction(Function("ln")) testStack),     1.945910149)
(*    | test 6 = floatEqual(top(getFunction(Function("min")) testStack),    5.0)
    | test 7 = floatEqual(top(getFunction(Function("max")) testStack),    7.0)*)
    | test 6 = floatEqual(top(getFunction(Function("abs")) testStack),    7.0)
    | test 7 = floatEqual(top(getFunction(Function("negate")) testStack), ~7.0)
    | test 8 = isFunction("sin")
    | test 9 = not (isFunction("t-dAwg"))
    | test 10 = isSymbolicOperator(#"+")
    | test 11 = not (isSymbolicOperator(#"="))
    | test _ = raise Domain
      val numberOfTests = 12
    in
      List.app result (List.tabulate(numberOfTests, fn n => (n, test)))
    end;


