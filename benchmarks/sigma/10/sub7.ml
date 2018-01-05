(* 2004-11957 "Computer science and engineering" "Park Kwang-seok" homework#1-1 *)

let rec sigma (a, b, f) =
	if a > b then 0
	else sigma ((a+1), b, f) + (f a)

(*
(* test code *)
let square a = a * a

let _ = print_endline (string_of_int (sigma (1, 5, square)))
*)
