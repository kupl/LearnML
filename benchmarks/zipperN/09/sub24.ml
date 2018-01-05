(*
 * Student no. : 2009-20769
 * Name        : Kim, Seongjun
 *)

let rec zipperN ll =
    let first_list = List.hd ll in
    let remain_list = List.tl ll in

    if ll = [[]] then []
    else 
        match first_list with
            [] -> zipperN remain_list
            | x :: xs -> x :: zipperN (List.append remain_list [xs])

(* TEST
open OUnit;;

let test_zipperN _ =
    Printf.printf "(1)";
    assert_equal [1;2;3] (zipperN [[1;2;3]]);
    Printf.printf "(2)";
    assert_equal [1;4;2;5;3;6] (zipperN [[1;2;3]; [4;5;6]]);
    Printf.printf "(3)";
    assert_equal [1;4;7;2;5;8;3;6;9] (zipperN [[1;2;3]; [4;5;6]; [7;8;9]]);
    Printf.printf "(4)";
    assert_equal [1;3;7;2;4;8;5;9;6] (zipperN [[1;2]; [3;4;5;6]; [7;8;9]]);
    Printf.printf "(5)";
    assert_equal [] (zipperN [[]]);
    Printf.printf "(6)";
    assert_equal [] (zipperN [[]; []; []])

let suite = "Test Excercise4" >::: ["test of zipperN" >:: test_zipperN]

let _ = run_test_tt_main suite
 *)
