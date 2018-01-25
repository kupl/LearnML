(* problem 4*)

let product : (int -> int) -> int -> int -> int
= fun f a b -> 
	let rec help f a b x =
		if a > b then x
		else if a == b then x*(f a)
		else help f (a+1) b (x*(f a)) in
			help f a b 1;;


(* problem 5*)

let dfact : int -> int
= fun n -> 
	if n mod 2 == 0 then product (fun x-> 2*x) 1 (n/2)
	else product (fun x-> 2*x-1) 1 ((n+1)/2)