use "tests_datatypes.sml";  val test_datatypes  = test;
use "tests_functions.sml";  val test_functions  = test;
use "tests_enviroment.sml"; val test_enviroment = test; 
use "tests_tokenize.sml";   val test_tokenize   = test;
use "tests_validate.sml";   val test_validate   = test;
use "tests_translate.sml";  val test_translate  = test;
use "tests_evaluate.sml";   val test_evaluate   = test;

fun heading(str) = (print("\n----------------------------------------\n");
   print(str^"\n"); print("----------------------------------------\n"));

(heading("test datatypes");  test_datatypes();
 heading("test functions");  test_functions();
 heading("test enviroment"); test_enviroment();
 heading("test tokenize");   test_tokenize();
 heading("test validate");   test_validate();
 heading("test translate");  test_translate();
 heading("test evaluate");   test_evaluate());
