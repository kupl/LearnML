let rec merge : int list * int list -> int list =
	function (a, b) -> 
	match (a, b) with
	| ([], _) -> b
	| (_, []) -> a
	| _ -> (
		if (List.hd a) < (List.hd b) then 
			(List.hd b) :: (merge (a, (List.tl b)))
		else 
			(List.hd a) :: (merge ((List.tl a), b))
	)
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
    test_merge [5;3;1] [6;4;2] [6;5;4;3;2;1]
*)
