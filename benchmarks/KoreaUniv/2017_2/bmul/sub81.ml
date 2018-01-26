(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> 
let rec dtob : int -> bin
=fun n -> 
	let a = n/2 in
		let b = n mod 2 in
			if a = 0 then [ONE]
				else if b=0 then ((dtob a) @ [ZERO])
							else ((dtob a) @ [ONE])
in let rec num : bin -> int
=fun b -> 
	match b with 
	| [] -> 0
	| hd::tl -> 1 + (num (tl))
in let rec btod : bin -> int -> int
= fun b n-> 
	let rec binexp : int -> int
	= fun a -> 
	if a =0 then 1 else 2*(binexp (a-1)) 
	in match b with
	| [] -> 0 
	| hd::tl -> if hd = ONE then binexp(n-1)+(btod tl (n-1)) else btod tl (n-1)
in dtob((btod b1 (num b1))*(btod b2 (num b2)))
