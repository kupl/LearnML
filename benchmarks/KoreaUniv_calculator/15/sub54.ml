type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e ->
match e with
| INT n -> n
| ADD (e1, e2) -> ((calculator e1) + (calculator e2))
| SUB (e1, e2) -> ((calculator e1) - (calculator e2))
| MUL (e1, e2) -> ((calculator e1) * (calculator e2))
| DIV (e1, e2) -> let v2 = calculator e2 in
			(match v2 with
			| 0 -> raise (Failure "Wrong Divisor")
			| _ -> ((calculator e1) / (calculator e2))
			)
| SIGMA(e1,e2,e3) -> (if ((calculator e1) = (calculator e2)) then (calsig e1 e3)
			else (let k1 = (calsig e1 e3) in
				let k2 = (let m1 = ADD(e1, INT 1) in
						let m2= SIGMA(m1,e2, e3) in 
						calculator m2 ) in
				(k1 + k2)
				)

			)
| X -> raise (Failure "wrong variable location")

and calsig: exp -> exp -> int
=fun e1 e3
-> let v1 = calculator e1 in
	(match v1 with
	| n ->(match e3 with 
		| INT n -> n
		| ADD (s1, s2) -> ((calsig e1 s1) + (calsig e1 s2))
		| SUB (s1, s2) -> ((calsig e1 s1) - (calsig e1 s2))
		| MUL (s1, s2) -> ((calsig e1 s1) * (calsig e1 s2))
		| DIV (s1, s2) ->( let v2 = calculator s2 in
				(match v2 with
				| 0 -> raise (Failure "Wrong Divisor")
				| _ -> ((calsig e1 s1) / (calsig e1 s2))
				)
				)
		| SIGMA(s1,s2,s3) -> (if ((calculator s1) = (calculator s2)) then (calsig s1 s3)
					else (let k1 = (calsig s1 s3) in
						let k2 = (let m2 = ADD(s1, INT 1) in
								SIGMA(m2, s2, s3)) in
						let m1 = ADD(INT k1, k2) in
						calculator m1
						)
					)
		| X -> (calculator e1)
		)
	)
