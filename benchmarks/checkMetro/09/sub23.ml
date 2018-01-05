(*
 * Student no. : 2009-20769
 * Name        : Kim, Seongjun
 *)
type metro = STATION of string
           | AREA of string * metro
           | CONNECT of metro * metro

let checkMetro m =
    let find_name name nlist = List.exists (fun n -> n = name) nlist in
    let rec check m nlist =
        match m with
            STATION n -> find_name n nlist
            | AREA (n, mm) -> check mm (n :: nlist)
            | CONNECT (m1, m2) -> check m1 nlist && check m2 nlist
    in
    check m []

(* TEST
open OUnit;;

let test_checkMetro _ =
    assert_equal true (checkMetro (AREA("a", STATION "a")));
    assert_equal true (checkMetro (AREA("a", AREA("a", STATION "a"))));
    assert_equal true (checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))));
    assert_equal true (checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a")))));

    assert_equal false (checkMetro (AREA("a", STATION "b")));
    assert_equal false (checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c")))));
    assert_equal false (checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c")))))

let suite = "Test Excercise" >::: ["test of checkMetro" >:: test_checkMetro]

let _ = run_test_tt_main suite
*)
