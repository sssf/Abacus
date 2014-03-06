(*
  Import needed file so this can be run on its own
*)
use "enviroment.sml";
use "datatypes.sml";
use "stack.sml";
use "functions.sml";
use "evaluate.sml";
use "tokenize.sml";
use "translate.sml";
use "input.sml";
use "validate.sml";
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

      (* valid(exp)
         TYPE: string -> bool
         PRE: true
         POST: true if exp is a valid expression, false otherwise
         Example: valid("1+1*2) = true
                  valid("1 1") = false
      *)
      fun valid(exp) = validate(tokenize(exp));


      (* test n
           TYPE: int -> bool
           PRE:  1<=n<=4
           POST: true iff test n executes correctly
       *)
      fun test 0 = (valid("1") andalso
                    valid("-1") andalso
                    valid("1 + 1") andalso
                    valid("1 + -1") andalso
                    valid("-1 + 1") andalso
                    valid("sin 3") andalso
                    valid("sin x")  andalso
                    valid("sin (x*2)") andalso
                    valid("sin cos x") andalso
                    valid("1 * (2 + 3)") andalso
                    valid("x = 10") andalso
                    valid("x = 10 * 2") andalso
                    valid("x = y") andalso
                    valid("x = y = 42"))
        | test 1 = (valid("++1") = false andalso
                    valid("1++") = false andalso
                    valid("1*+1") = false andalso
                    valid("1 sin") = false andalso
                    valid("* sin 1") = false andalso
                    valid("00") = false andalso
                    valid("-1 * sin sin sin sin sin sin") = false)
        | test _ = raise Domain
      val numberOfTests = 2
    in
      List.app result (List.tabulate(numberOfTests, fn n => (n, test)))
    end;
