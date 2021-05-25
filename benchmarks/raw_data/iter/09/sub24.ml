(*
 * Student no. : 2009-20769
 * Name        : Kim, Seongjun
 *)

let rec iter (n, f) =
    let cat_fun f1 f2 =
        fun x -> f1 (f2 x)
    in

    let identity x =
        x
    in

    if n = 0 then 
        identity
    else
        cat_fun f (iter ((n-1), f))

(* TEST
open OUnit;;
let test_iter _ =
    Printf.printf "(1)";
    assert_equal 1234 ((iter (0, (fun n -> n * n))) 1234);
    Printf.printf "(2)";
    assert_equal 16 ((iter (2, (fun n -> n * n))) 2);
    Printf.printf "(3)";
    assert_equal 200 ((iter (100, ((+) 2))) 0);
    Printf.printf "(4)";
    assert_equal 20000 ((iter (10000, ((+) 2))) 0);
    Printf.printf "(5)";
    assert_equal [3;4] ((iter (2, List.tl)) [1;2;3;4])
 
let suite = "Test Excercise2" >::: ["test of iter" >:: test_iter]

let _ = run_test_tt_main suite
 *)
