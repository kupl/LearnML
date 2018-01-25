(* problem 2*)

let smallest_divisor : int -> int
= fun n -> 
	let rec f a b =
		if a < b*b then a
		else if a mod b == 0 then b
		else f a (b+1)
	in f n 2