type ae = CONST of int
		| VAR of string
		| POWER of string * int
		| TIMES of ae list
		| SUM of ae list
exception InvalidArgument

let rec diff (ae, str) =
		match ae with
		| CONST i -> CONST 0
		| VAR s ->
			if s = str then CONST 1
			else CONST 0
		| POWER (s, i) ->
			if s = str then TIMES ([CONST i] @ [POWER (s, i-1)])
			else CONST 0
		| TIMES (hd::tl) -> 
			if tl = [] then diff (hd, str)
			else SUM ([TIMES ([diff (hd, str)] @ tl)] @ [TIMES ([hd] @ [diff (TIMES tl, str)])])
		| TIMES [] -> raise InvalidArgument
		| SUM (hd::tl) ->
			if tl = [] then diff (hd, str)
			else SUM ([diff (hd, str)] @ [diff (SUM tl, str)])
		| SUM [] -> raise InvalidArgument
