(*
 * Student no. : 2009-20769
 * Name        : Kim, Seongjun
 *)
type lambda = V of string
           | P of string * lambda
           | C of lambda * lambda

let check m =
    let find_var var nlist = List.exists (fun n -> n = var) nlist in
    let rec check m nlist =
        match m with
            V n -> find_var n nlist
            | P (n, mm) -> check mm (n :: nlist)
            | C (m1, m2) -> check m1 nlist && check m2 nlist
    in
    check m []

(* TEST
open OUnit;;

let test_check _ =
    assert_equal true (check (P("a", V "a")));
    assert_equal true (check (P("a", P("a", V "a"))));
    assert_equal true (check (P("a", P("b", C(V "a", V "b")))));
    assert_equal true (check (P("a", C(V "a", P("b", V "a")))));

    assert_equal false (check (P("a", V "b")));
    assert_equal false (check (P("a", C(V "a", P("b", V "c")))));
    assert_equal false (check (P("a", P("b", C(V "a", V "c")))))

let suite = "Test Excercise" >::: ["test of check" >:: test_check]

let _ = run_test_tt_main suite
*)
