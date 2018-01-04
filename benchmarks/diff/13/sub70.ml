type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

exception InvalidArgument

let rec diff: ae * string -> ae
	= fun (alexp, dv) ->
		match alexp with
		| CONST i -> CONST 0
		| VAR str -> if (str = dv) then CONST 1 else CONST 0
		| POWER (str, i) -> if (str = dv) then TIMES(CONST i::POWER(str, i - 1)::[]) else CONST 0
		| TIMES [] | SUM [] -> raise InvalidArgument
		| TIMES (hd::[]) -> diff(hd, dv)
		| TIMES (hd::tl) -> SUM (TIMES (diff(hd, dv)::tl)::TIMES (hd::diff(TIMES tl, dv)::[])::[])
		| SUM alexpList -> SUM(List.map (fun alexp -> diff (alexp, dv)) alexpList)
