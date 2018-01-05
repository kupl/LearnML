let rec merge l1 l2 = match (l1, l2) with
	| ([], _) -> l2
	| (_, []) -> l1
	| (h1::t1, h2::t2) -> if h1>h2 then h1::(merge t1 l2) else h2::(merge l1 t2) 

(*let _ = List.iter print_endline (List.map string_of_int (merge [9;6;3;1] [4;8;2])) *)
