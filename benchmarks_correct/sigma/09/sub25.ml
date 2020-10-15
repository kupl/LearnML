(*
 * Student no. : 2009-20769
 * Name        : Kim, Seongjun
 *)

let rec sigma f a b =
    if a > b then 0
    else (f a) + (sigma f (a+1) b)

(* TEST 
open OUnit;;

let test_sigma _ =
    assert_equal 0 (sigma (1, 0, (fun n -> n)));
    assert_equal 55 (sigma (1, 10, (fun n -> n)));
    assert_equal 30 (sigma (1, 4, (fun n-> n*n)));
    assert_equal 1 (sigma (1, 1, (fun n -> n)))

let suite = "Test Excercise1" >::: ["test of sigma" >:: test_sigma]

let _ = run_test_tt_main suite
 *)
