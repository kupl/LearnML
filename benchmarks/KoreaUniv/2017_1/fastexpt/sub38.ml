(* problem 1*)

let rec fastexpt : int -> int -> int
= fun b n ->
	if n ==1 then b else
	if n mod 2 == 0 then
		let a = fastexpt b (n/2) in a*a
	else
		let a = fastexpt b (n/2) in a*a*b