use "tests_datatypes.sml";  val test_datatypes  = test;
use "tests_functions.sml";  val test_functions  = test;
use "tests_enviroment.sml"; val test_enviroment = test;
use "tests_tokenize.sml";   val test_tokenize   = test;
use "tests_validate.sml";   val test_validate   = test;
use "tests_translate.sml";  val test_translate  = test;
use "tests_evaluate.sml";   val test_evaluate   = test;





(*   run_test(title, test)
     TYPE: string * (unit -> 'a) -> 'a
     PRE:  true
     POST: acc reversed and converted to string
     EXAMPLE: accToString([#"s", #"n", #"a"]) = "ans"
  *)
fun run_test(title, test) =
  let
    val hline = "\n----------------------------------------\n"
  in
    (print(hline^"Test: "^title^hline); test())
  end;



(run_test("datatypes",  test_datatypes);
 run_test("functions",  test_functions);
 run_test("enviroment", test_enviroment);
 run_test("tokenize",   test_tokenize);
 run_test("validate",   test_validate);
 run_test("translate",  test_translate);
 run_test("evaluate",   test_evaluate));

