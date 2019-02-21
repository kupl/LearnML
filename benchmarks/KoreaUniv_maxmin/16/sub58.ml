(*********************)
(*     Problem 1     *)
(*********************)

let rec max : int list -> int
= fun lst -> match lst with
| [] -> raise (Failure "The list is too short")
| [a] -> a
| a :: b :: tl ->
	if a < b then max (b :: tl)
	else max (a :: tl) 

let rec min : int list -> int
= fun lst -> match lst with
| [] -> raise (Failure "The list is too short")
| [a] -> a
| a :: b :: tl ->
	if a < b then min (a :: tl)
	else min (b :: tl) 
