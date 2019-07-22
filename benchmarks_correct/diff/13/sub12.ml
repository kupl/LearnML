exception InvalidArgument

type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

let rec diff (ae,x) =
	match ae with CONST _ -> CONST 0
	| VAR v -> if (v=x) then CONST 1 else CONST 0
	| POWER (v,p) ->  if (v=x) then TIMES [CONST p; POWER(v,p-1)] else CONST 0
	| TIMES lst ->
		(
		match lst with [] -> raise InvalidArgument
		| head::[] -> diff (head, x)
		| head::tail -> SUM [ TIMES ((diff (head, x))::tail); TIMES (head::[diff(TIMES tail,x)]) ]
		)
	| SUM lst ->
		(
		match lst with [] -> raise InvalidArgument
		| head::[] -> diff (head, x)
		| head::tail -> SUM ((diff (head, x))::[diff(SUM tail,x)])
		)
