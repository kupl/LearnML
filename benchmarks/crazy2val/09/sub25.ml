(*
 * Student no. : 2009-20769
 * Name        : Kim, Seongjun
 *)

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val c =
    match c with
        NIL -> 0
        | ZERO c2 -> 2 * (crazy2val c2)
        | ONE c2 -> 1 + (2 * (crazy2val c2))
        | MONE c2 -> -1 + (2 * (crazy2val c2))

(* TEST
open OUnit;;

let rec crazy2_of_string s =
    match s with
        [] -> NIL
        | '+' :: ss -> ONE (crazy2_of_string ss)
        | '-' :: ss -> MONE (crazy2_of_string ss)
        | '0' :: ss -> ZERO (crazy2_of_string ss)
        | _ -> NIL

let test_crazy2val _ =
    Printf.printf "(1)";
    assert_equal 0 (crazy2val (crazy2_of_string []));
    Printf.printf "(2)";
    assert_equal 0 (crazy2val (crazy2_of_string ['0']));
    Printf.printf "(3)";
    assert_equal 1 (crazy2val (crazy2_of_string ['+']));
    Printf.printf "(4)";
    assert_equal (-1) (crazy2val (crazy2_of_string ['-']));
    Printf.printf "(5)";
    assert_equal 15 (crazy2val (crazy2_of_string ['+';'+';'+';'+']));
    Printf.printf "(6)";
    assert_equal (-15) (crazy2val (crazy2_of_string ['-';'-';'-';'-']));
    Printf.printf "(7)";
    assert_equal 0 (crazy2val (crazy2_of_string ['0';'0';'0';'0']));
    Printf.printf "(8)";
    assert_equal 6 (crazy2val (crazy2_of_string ['0';'+';'-';'+']))

let suite = "Test Excercise6" >::: ["test of crazy2val" >:: test_crazy2val]

let _ = run_test_tt_main suite
 *)
