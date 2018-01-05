(* let rec zip ((ilist1: int list), (ilist2: int list)) : int list =
	match (ilist1, ilist2) with
	| (([]), _) -> ilist2
	| (_, ([])) -> ilist1
	| ((hd1::tl1), (hd2::tl2)) -> hd1::hd2::(zip (tl1, tl2))  *)

let rec merge ((ilist1: int list), (ilist2: int list)) : int list = 
	match (ilist1, ilist2) with
	| (([]), _) -> ilist2
	| (_, ([])) -> ilist1
	| ((hd1::tl1), (hd2::tl2)) -> if hd1 > hd2 then hd1::(merge (tl1, ilist2)) else hd2::(merge (ilist1, tl2))


(* open Printf
let a = [5;4;3]
let b = [12;10;2;1]
let c = merge (a, b)
let () = List.iter (printf "%d ") c; print_newline () *)



(* let () = List.iter (printf "%d ") (merge ([5; 3; 1], [6; 2; 1])); print_newline ()
let () = List.iter (printf "%d ") (merge ([5; 2], [6; 5; 5])); print_newline ()
let () = List.iter (printf "%d ") (merge ([], [])); print_newline ()
let () = List.iter (printf "%d ") (merge ([1], [])); print_newline ()
let () = List.iter (printf "%d ") (merge ([], [2])); print_newline ()
let () = List.iter (printf "%d ") (merge ([3;2;1], [4;3;2])); print_newline ()
let () = List.iter (printf "%d ") (merge ([2;1], [10;9])); print_newline ()
let () = List.iter (printf "%d ") (merge ([10;9], [4;3;2])); print_newline ()
let () = List.iter (printf "%d ") (merge ([5;3;1], [6;4;2])); print_newline () *)


(* print_test_equal ~test_name:("merge2") str_conv [6; 5; 5; 5; 2] (merge ([5; 2], [6; 5; 5]));
print_test_equal ~test_name:("merge3") str_conv [] (merge ([], []));
print_test_equal ~test_name:("merge4") str_conv [1] (merge ([1], []));
print_test_equal ~test_name:("merge5") str_conv [2] (merge ([], [2]));
print_test_equal ~test_name:("merge6") str_conv [4;3;3;2;2;1] (merge ([3;2;1], [4;3;2]));
print_test_equal ~test_name:("merge7") str_conv [10;9;2;1] (merge ([2;1], [10;9]));
print_test_equal ~test_name:("merge8") str_conv [10;9;4;3;2] (merge ([10;9], [4;3;2]));
print_test_equal ~test_name:("merge9") str_conv [6;5;4;3;2;1] (merge ([5;3;1], [6;4;2])); *)