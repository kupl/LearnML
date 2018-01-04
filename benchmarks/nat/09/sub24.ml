(*
 * Student no. : 2009-20769
 * Name        : Kim, Seongjun
 *)

type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
    match b with
        ZERO -> a
        | SUCC nat -> natadd ((SUCC a), nat)

let rec natmul (a, b) =
    match b with
        ZERO -> ZERO
        | SUCC nat -> natadd (a, (natmul (a, nat)))

(* TEST
open OUnit;;

let rec create_nat i =
    if i = 0 then ZERO
    else SUCC (create_nat (i-1))

let test_natadd _ =
    Printf.printf "(1)";
    assert_equal (create_nat 0) (natadd ((create_nat 0), (create_nat 0)));
    Printf.printf "(2)";
    assert_equal (create_nat 3) (natadd ((create_nat 0), (create_nat 3)));
    Printf.printf "(3)";
    assert_equal (create_nat 3) (natadd ((create_nat 3), (create_nat 0)));
    Printf.printf "(4)";
    assert_equal (create_nat 5) (natadd ((create_nat 2), (create_nat 3)))

let test_natmul _ =
    Printf.printf "(1)";
    assert_equal (create_nat 0) (natmul ((create_nat 0), (create_nat 0)));
    Printf.printf "(2)";
    assert_equal (create_nat 0) (natmul ((create_nat 0), (create_nat 3)));
    Printf.printf "(3)";
    assert_equal (create_nat 0) (natmul ((create_nat 3), (create_nat 0)));
    Printf.printf "(4)";
    assert_equal (create_nat 3) (natmul ((create_nat 3), (create_nat 1)));
    Printf.printf "(5)";
    assert_equal (create_nat 3) (natmul ((create_nat 1), (create_nat 3)));
    Printf.printf "(6)";
    assert_equal (create_nat 6) (natmul ((create_nat 2), (create_nat 3)))

let suite = "Test Excercise5" >::: 
    ["test of natadd" >:: test_natadd; "test of natmul" >:: test_natmul]

let _ = run_test_tt_main suite
 *)
