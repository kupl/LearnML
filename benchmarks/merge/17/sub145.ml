(*Computer Engineering 2015-12683 Kim Jaein Exercise 1-1*)
let rec merge ((a: int list), (b: int list)) =
	if List.length a == 0 then b
	else if List.length b == 0 then a
	else if List.hd a > List.hd b then List.hd a :: merge ((List.tl a), b)
	else List.hd b :: merge (a, (List.tl b))
(*
open Printf
let () = List.iter (printf "%d ") (merge ([12; 3;], [])); print_newline()
*)
