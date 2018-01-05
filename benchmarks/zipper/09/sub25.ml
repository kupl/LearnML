(*
 * Student no. : 2009-20769
 * Name        : Kim, Seongjun
 *)

let rec zipper (x, y) =
    match x with
        [] -> y
        | x_head :: x_tail -> x_head :: zipper(y, x_tail)
        
(* TEST
open OUnit;;

let test_zipper _ =
    Printf.printf "(1)";
    assert_equal [] (zipper ([], []));
    Printf.printf "(2)";
    assert_equal [1;2;3] (zipper ([], [1;2;3]));
    Printf.printf "(3)";
    assert_equal [1;2;3] (zipper ([1;2;3], []));
    Printf.printf "(4)";
    assert_equal [1;4;2;5;3;6] (zipper ([1;2;3], [4;5;6]));
    Printf.printf "(5)";
    assert_equal [1;4;2;5;3;6;7] (zipper ([1;2;3], [4;5;6;7]));
    Printf.printf "(6)";
    assert_equal [0;4;1;5;2;6;3] (zipper ([0;1;2;3], [4;5;6]))

let suite = "Test Excercise3" >::: ["test of zipper" >:: test_zipper]

let _ = run_test_tt_main suite
 *)
