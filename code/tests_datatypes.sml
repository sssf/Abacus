use "enviroment.sml";
use "datatypes.sml";


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

      val testEnviroment1 = Enviroment([("Pi", Math.pi),
                                    ("e",  Math.e),("ans", 42.0)])
      val testEnviroment2 = Enviroment([("ans", 80.08),("Pi", Math.pi),
                                    ("e",  Math.e)])
      val testEnviroment3 = Enviroment([("ans", 80.01234567891011121314),("Pi", Math.pi),
                                    ("e",  Math.e)])
  (* test n
       TYPE: int -> bool
       PRE:  1<=n<=4
       POST: true iff test n executes correctly
   *)
  fun test 0 = format(testEnviroment1) = "42.0\n"(*test 1*)
    | test 1 = format(testEnviroment2) = "80.08\n"(*Same as test 1, but the variable "ans" is stored differently in enviroment*)
    | test 2 = not (format(testEnviroment3) = "80.01234567891011121314\n")(*Too many decimals, cant handle all of them*)
    | test _ = raise Domain
      val numberOfTests = 3
    in
      List.app result (List.tabulate(numberOfTests, fn n => (n, test)))
    end;


