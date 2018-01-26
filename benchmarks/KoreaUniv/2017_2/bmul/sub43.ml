(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> (* TODO *)
	let bnot d = if d=ZERO then ONE else ZERO in
	let fulladd d1 d2 d3 =
		if d1=ZERO then
			if d2=ZERO then (ZERO,d3) else (d3,bnot d3)
		else if d2=ZERO then (d3,bnot d3) else (ONE,d3)
	in
	let rec adder b1 b2 cr =
		match b1 with
		| [] -> (
			match b2 with
			| [] -> if cr=ONE then [ONE] else []
			| h2::t2 ->
				match fulladd ZERO h2 cr with (a,b) ->
					b::(adder b1 t2 a))
		| h1::t1 ->
			match b2 with
			| [] -> (match fulladd ZERO h1 cr with
				(a,b) -> b::(adder t1 b2 a))
			| h2::t2 ->
				match fulladd h1 h2 cr with
				(a,b) -> b::(adder t1 t2 a)
	in
	let add b1 b2 = List.rev (adder (List.rev b1) (List.rev b2) ZERO) in
	let rec mul b1 b2 ans =
		match b2 with
		| [] -> ans
		| hd::tl -> 
			let newans = add ans ans in
			if hd=ZERO then mul b1 tl newans
			else mul b1 tl (add b1 newans)
	in
		mul b1 b2 [ZERO]