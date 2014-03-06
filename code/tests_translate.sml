(*
  Import needed file so this can be run on its own
*)
use "stack.sml";
use "enviroment.sml";
use "datatypes.sml";
use "functions.sml";
use "translate.sml";
(*
  Template for writing test cases. Based on assignment3_tests.sml by Dave Clarke


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
       PRE:  1<=n<=4
       POST: true iff test n executes correctly
   *)
  fun test 0 = translate([]) = []
    | test 1 = translate([Open,Open,Open,Open,Variable("x"),Closed,Closed,Closed,Closed]) = [Variable("x")]
    | test 2 = translate([Number("1"),Operator("+"),Number("1")]) = [Number("1"),Number("1"),Operator("+")]
    | test 3 = translate([Negate,Function("cos"),Function("sin"),Open,Variable("x"),Operator("*"),Number("1"),Closed])
                            =  [Variable "x", Number "1", Operator "*", Function "sin", Function "cos", Function "negate"]
    | test _ = raise Domain
      val numberOfTests = 4
    in
      List.app result (List.tabulate(numberOfTests, fn n => (n, test)))
    end;
