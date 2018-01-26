(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> (* TODO *) 
	let rec calc e v =
		match e with
		| X -> v
		| INT (i) -> i
		| ADD (e1,e2) -> calc e1 v + calc e2 v
		| SUB (e1,e2) -> calc e1 v - calc e2 v
		| MUL (e1,e2) -> calc e1 v * calc e2 v
		| DIV (e1,e2) -> calc e1 v / calc e2 v
		| SIGMA (e1,e2,e3) ->
			let i1 = calc e1 v in
			let i2 = calc e2 v in
			if i1>i2 then 0 
			else if i1=i2 then calc e3 i1
			else calc e3 i1 + calc (SIGMA(INT (i1+1),INT i2,e3)) v
	in
		calc e 0
