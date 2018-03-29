(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> 
	if amount < 0 then 0 (* if amount is negative, return 0 *)
	else if amount == 0 then 1 (* if amount is zero, return 1 *)
	else if (List.length coins) == 1 then (* if coins is a singleton list, if amount is divisible by the coin, return 1 else 0 *)
	(
		match coins with
		| [] -> 0
		| hd::tl -> 
			if (amount mod hd) == 0 then 1 else 0
	)
	else
	(
		let coins_desc = List.rev (List.sort compare coins) in (* coins in descending order *)
		match coins_desc with
		[] -> 0
		| hd::tl ->
		(
			let result = ref 0 in
			for i = 0 to amount / hd do
				result := !result + (change tl (amount - (hd * i)))
			done;
			!result
		)		
	)
;;