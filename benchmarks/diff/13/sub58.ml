type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

exception InvalidArgument

let rec diff(ae,str) =
	match ae with
	| CONST c -> CONST (0)
	| VAR s -> if(s=str) then CONST (1)
		else CONST (0)
	| POWER (s, c) ->  if(s=str) then TIMES[POWER(s, c-1);CONST (c)]
		else CONST (0)
	| TIMES li ->
	(
		match li with
		| [] -> raise InvalidArgument
		| [head] -> diff(head, str)
		| head :: tail -> SUM[TIMES[diff(head, str);TIMES(tail)] ; TIMES[head ; diff(TIMES(tail), str)]]
	)
	| SUM l ->
	(
		match l with
		| [] -> raise InvalidArgument
		| [head] -> diff(head, str)
		| head :: tail -> SUM[diff(head, str) ; diff(SUM(tail), str)]
	)
