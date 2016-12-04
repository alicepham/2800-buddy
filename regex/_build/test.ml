open OUnit2
open Ast
open Regex

let tests = [

"regex test 1" >:: (fun _ -> assert_equal "(a+b)" (parse "a+b" |> print));
"regex test 2" >:: (fun _ -> assert_equal "(a)*" (parse "a*" |> print));
"regex test 3" >:: (fun _ -> assert_equal "(ab)" (parse "ab" |> print));
"regex test 4" >:: (fun _ -> assert_equal "((a(a)*)+(b((a+b))*))"
                                (parse "aa* + b(a+b)*" |> print));
"regex test 5" >:: (fun _ -> assert_equal "E" (parse "E" |> print));

"star_rule1 test1" >:: (fun _ -> assert_equal "(((a+b))*(a+b))"
                          (parse "(a+b)(a+b)*" |> star_rule |> print));

]



let _ = run_test_tt_main ("suite" >::: tests)