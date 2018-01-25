(* problem 5*)

let dfact : int -> int
= fun n ->
	let rec f n a =
		if n == 1 || n == 2 then a*n
		else f (n-2) (a*n)
	in f n 1