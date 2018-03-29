(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> 
	let rec reverse = fun l ->
		match l with 
		| [] -> []
		| hd::tl -> (reverse tl)@[hd]
	in if amount = 0 then 1
	else if amount < 0 then 0
	else match (reverse coins) with
			| [] -> 0
			| hd::tl -> (change coins (amount-hd)) + (change tl amount);;

