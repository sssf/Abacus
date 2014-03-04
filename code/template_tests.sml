(*
  Template for writing test cases. Based on assignment3_tests.sml by Dave Clarke


To run these training cases:
   1) launch PolyML shell [poly]
   2) load hand-in [use "crypto.sml"] TODO:
   3) load training set [use "template_tests.sml"]
   4) run tests [test ()]
*)



(* check function types *) (*
tokenize : string -> token list;
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
  fun test 0 = true
    | test 1 = false
    | test 2 = raise Fail "bläää"
    | test _ = raise Domain
      val numberOfTests = 2
    in
      List.app result (List.tabulate(numberOfTests, fn n => (n, test)))
    end;


