(* Problem 3 *)
let rec max : int list -> int
=fun l -> match l with
					| [] -> 0
					| x :: [] -> x
					| x :: tail -> 
						 let v = max tail in
						 if x > v then x
						 else v

let rec min : int list -> int
=fun l -> match l with
				| [] -> 0
				| x :: [] -> x
				| x :: tail ->
					 let v = min tail in
					 if x < v then x
					 else v
