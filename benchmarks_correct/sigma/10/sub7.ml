(* 2004-11957 "Computer science and engineering" "Park Kwang-seok" homework#1-1 *)

let rec sigma f a b =
	if a > b then 0
	else sigma f (a+1) b + (f a)

(*
(* test code *)
let square a = a * a

let _ = print_endline (string_of_int (sigma (1, 5, square)))
*)
