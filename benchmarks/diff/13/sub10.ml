exception InvalidArgument

type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

let diff : ae * string -> ae = fun (e, str) ->
	let rec diff_sub (e, str) =
		match e with
		| CONST n -> CONST 0
		| VAR s -> 
		    if s = str then CONST 1
		    else CONST 0
		| POWER (str, n) -> TIMES [CONST n; POWER (str, (n-1))]
		| TIMES lst -> ( 
			match lst with
			| [] -> raise InvalidArgument
			| a::[] -> diff_sub (a, str)
			| a::t -> SUM [TIMES [diff_sub (a, str); TIMES t]; TIMES[a; diff_sub (TIMES t, str)]]
		)
		| SUM lst -> 
			match lst with
			| [] -> raise InvalidArgument
			| a::[] -> diff_sub (a, str)
			| a::t -> SUM [diff_sub (a, str); diff_sub ((SUM t), str)]
	in
	diff_sub (e, str)
