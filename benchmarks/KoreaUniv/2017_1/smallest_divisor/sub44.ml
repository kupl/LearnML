(* problem 2*)

let rec smallest_divisor : int -> int
= fun n ->
	let m=2 in
		let rec for_loop n m = if n<m*m then n else if n mod m = 0 then m else for_loop n (m+1) in for_loop n m;;