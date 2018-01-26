(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> 
	let rec reverse list =
	match list with
	| [] -> []
	| hd :: tl -> (reverse tl) @ [hd] in

	let rec binadd b1 b2 carry =
	match b1, b2 with
	| [], [] when carry = ZERO -> []
	| [], [] (* when carry = ONE *) -> [ONE]
	| [], (hd :: tl) -> binadd [ZERO] b2 carry
	| (hd :: tl), [] -> binadd b1 [ZERO] carry
	| (hd1 :: tl1), (hd2 :: tl2) ->
		match hd1, hd2, carry with
		| ZERO, ZERO, ZERO 
		-> ZERO :: (binadd tl1 tl2 ZERO)
		| ZERO, ZERO, ONE | ZERO, ONE, ZERO | ONE, ZERO, ZERO
		-> ONE :: (binadd tl1 tl2 ZERO)
		| ZERO, ONE, ONE | ONE, ZERO, ONE | ONE, ONE, ZERO 
		-> ZERO :: (binadd tl1 tl2 ONE)
		| ONE, ONE, ONE
		-> ONE :: (binadd tl1 tl2 ONE) in

	let rec binmul b1 b2 =
		match b2 with
		| [] -> [ZERO]
		| hd :: tl when hd = ZERO -> binmul (ZERO :: b1) tl
		| hd :: tl (* when hd = ONE *) -> binadd b1 (binmul (ZERO :: b1) tl) ZERO
	
	in match b1, b2 with
	| [], _ | _, [] -> raise (Failure "NullListError: one or both list are null")
	| _, _ -> reverse (binmul (reverse b1) (reverse b2))