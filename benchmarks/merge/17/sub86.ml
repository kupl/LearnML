(* Mechanical & Aerospace Eng./2013-11706/Kang Injae/1-1.ml *)

let rec merge ((ilist1 : int list), (ilist2 : int list)) : int list =
  match (ilist1, ilist2) with
  | ([], _) -> ilist2
  | (_, []) -> ilist1
  | (hd1::tl1, hd2::tl2) ->
      if hd1>hd2 then hd1::(merge(tl1, ilist2))
      else hd2::(merge(tl2, ilist1))
(*
let _ =
    let rec string_of_list = function
        [] -> ""
        | e::l -> string_of_int e ^ " " ^ string_of_list l
    in
    let assert_equal (expected: int list) (actual: int list) =
        if expected = actual then print_endline "true"
        else
            let expected_string = string_of_list expected in
            let actual_string = string_of_list actual in
            Printf.printf "Expected %s but actual %s\n" expected_string actual_string
    in
    let test_merge (xs: int list) (ys: int list) (expected: int list) =
        merge (xs, ys) |> assert_equal expected
    in
    test_merge [3;2;1] [4;3;2] [4;3;3;2;2;1];
    test_merge [2;1] [10;9] [10;9;2;1];
    test_merge [10;9] [4;3;2] [10;9;4;3;2];
    test_merge [5;3;1] [6;4;2] [6;5;4;3;2;1];
    test_merge [5;3;1] [6;2;1] [6;5;3;2;1;1];
    test_merge [5;2] [6;5;5] [6;5;5;5;2];
    test_merge [] [] [];
    test_merge [1] [] [1];
    test_merge [] [2] [2];
*)
