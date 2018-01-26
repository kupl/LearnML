(* Problem 3 *)
let rec prime n =
	let rec divisor d = 
	d > n/2 || (divisor (d+1) && n mod d <> 0) in 
  n <> 1 && divisor 2
