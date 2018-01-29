(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec shift n b 
= if n < 0 then [] else (shift (n-1) [ZERO]@b)
	
let rec reverse b =
	match b with
		| hd::tl -> (reverse tl)@[hd]
		| [] -> []

let rec reversed_add b1 b2 carry
= match b1 with
	| hd::tl -> (match hd with
			| ZERO -> (match b2 with
					| h::t -> (match h with
							| ZERO -> if carry = 0 then [ZERO]@(reversed_add tl t 0) else [ONE]@(reversed_add tl t 0)
							| ONE -> if carry = 0 then [ONE]@(reversed_add tl t 0) else [ZERO]@(reversed_add tl t 1))
					| [] -> if carry = 0 then [ZERO]@(reversed_add tl [] 0) else [ONE]@(reversed_add tl [] 0))
			| ONE -> (match b2 with
					| h::t -> (match h with
							| ZERO -> if carry = 0 then [ONE]@(reversed_add tl t 0) else [ZERO]@(reversed_add tl t 1)
							| ONE -> if carry = 0 then [ZERO]@(reversed_add tl t 1) else [ONE]@(reversed_add tl t 1))
					| [] -> if carry = 0 then [ONE]@(reversed_add tl [] 0) else [ZERO]@(reversed_add tl [] 1)))
	| [] -> (match b2 with 
		| hd::tl -> (match hd with
			| ZERO -> if carry = 0 then [ZERO]@(reversed_add [] tl 0) else [ONE]@(reversed_add [] tl 0)
			| ONE -> if carry = 0 then [ONE]@(reversed_add [] tl 0) else [ZERO]@(reversed_add [] tl 1))
		| [] -> if carry = 1 then [ONE] else [])


let rec reversed_multiply b1 b2 n
= match b1 with
	| hd::tl -> if hd = ONE then (reversed_add (shift n b2) (reversed_multiply tl b2 (n+1)) 0) else (reversed_add [ZERO] (reversed_multiply tl b2 (n+1)) 0)
	| [] -> [ZERO]

let rec remove_msb_zero b =
	match b with
		| hd::tl -> if hd = ZERO then remove_msb_zero tl else b
		| [] -> [ZERO]

let bmul : bin -> bin -> bin
= fun b1 b2 -> match b1 with
			| hd::tl -> (match b2 with
					| hd::tl -> remove_msb_zero (reverse(reversed_multiply (reverse b1) (reverse b2) 0))
					| [] -> remove_msb_zero (reverse(reversed_multiply (reverse b1) [ZERO] 0)))
			| [] -> (match b2 with
					| hd::tl -> remove_msb_zero (reverse(reversed_multiply [ZERO] (reverse b2) 0))
					| [] -> remove_msb_zero (reverse(reversed_multiply [ZERO] [ZERO] 0)))

