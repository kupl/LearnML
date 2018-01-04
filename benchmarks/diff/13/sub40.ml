type ae = CONST of int
					| VAR of string
					| POWER of string * int
					| TIMES of ae list
					| SUM of ae list

exception InvalidArgument (* if argument of TIMES and SUM is an empty list *)

let rec diff = fun (ae, str) ->
	match ae with
		CONST(i) -> CONST 0
		| VAR(i) -> if i = str then CONST 1 else CONST 0
		| POWER(i1, i2) -> if i1 = str then (
													if i2 = 1 then CONST 1
													else if i2 = 0 then CONST 0
													else TIMES [CONST i2; POWER (i1, i2 - 1)]
													)
												else CONST 0
		| TIMES(i) -> (match i with
			[] -> raise InvalidArgument
			| h::t -> if t=[] then diff(h, str)
								else SUM [TIMES ([diff(h, str)]@t); TIMES [h; diff(TIMES t, str)]])
		| SUM(i) -> (match i with
			[] -> raise InvalidArgument
			| h::t -> if t=[] then diff(h, str)
								else (SUM [diff(h, str); diff(SUM t, str)]))
