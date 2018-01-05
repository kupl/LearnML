(*
 * Student no. : 2009-20769
 * Name        : Kim, Seongjun
 *)

(* 6.ml *)
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val c =
    match c with
        NIL -> 0
        | ZERO c2 -> 2 * (crazy2val c2)
        | ONE c2 -> 1 + (2 * (crazy2val c2))
        | MONE c2 -> -1 + (2 * (crazy2val c2))
(* 6.ml end *)

let rec crazy2add (x, y) =
    match (x, y) with 
        _, NIL -> x
        | NIL, _ -> y
        | ONE xs, ONE ys -> ZERO (crazy2add ((crazy2add (xs, (ONE NIL))), ys))
        | MONE xs, MONE ys -> ZERO (crazy2add ((crazy2add (xs, (MONE NIL))), ys))
        | ZERO xs, ONE ys | ONE xs, ZERO ys -> ONE (crazy2add (xs, ys))
        | ZERO xs, MONE ys | MONE xs, ZERO ys -> MONE (crazy2add (xs, ys))
        | ONE xs, MONE ys | MONE xs, ONE ys | ZERO xs, ZERO ys -> ZERO (crazy2add (xs, ys))

(* TEST
open OUnit;;

let rec crazy2_of_string s =
    match s with
        [] -> NIL
        | '+' :: ss -> ONE (crazy2_of_string ss)
        | '-' :: ss -> MONE (crazy2_of_string ss)
        | '0' :: ss -> ZERO (crazy2_of_string ss)
        | _ -> NIL

let test_crazy2add _ =
    let z1 = crazy2_of_string [] in
    let z2 = crazy2_of_string ['+'] in
    let z3 = crazy2_of_string ['-'] in
    let z4 = crazy2_of_string ['0'] in
    let z5 = crazy2_of_string ['+';'+'] in
    let z6 = crazy2_of_string ['-';'-'] in
    let z7 = crazy2_of_string ['0';'0'] in
    Printf.printf "/";
    assert_equal (crazy2val (crazy2add(z1,z1))) (crazy2val(z1) + crazy2val(z1));
    Printf.printf "/";
    assert_equal (crazy2val (crazy2add(z2,z2))) (crazy2val(z2) + crazy2val(z2));
    Printf.printf "/";
    assert_equal (crazy2val (crazy2add(z3,z3))) (crazy2val(z3) + crazy2val(z3));
    Printf.printf "/";
    assert_equal (crazy2val (crazy2add(z4,z4))) (crazy2val(z4) + crazy2val(z4));
    Printf.printf "/";
    assert_equal (crazy2val (crazy2add(z2,z3))) (crazy2val(z2) + crazy2val(z3));
    Printf.printf "/";
    assert_equal (crazy2val (crazy2add(z5,z5))) (crazy2val(z5) + crazy2val(z5));
    Printf.printf "/";
    assert_equal (crazy2val (crazy2add(z6,z6))) (crazy2val(z6) + crazy2val(z6));
    Printf.printf "/";
    assert_equal (crazy2val (crazy2add(z7,z7))) (crazy2val(z7) + crazy2val(z7))

let suite = "Test Excercise7" >::: ["test of crazy2add" >:: test_crazy2add]

let _ = run_test_tt_main suite
 *)
