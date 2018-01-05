exception Error of string

type ae = CONST of int
		| VAR of string
		| POWER of string * int
		| TIMES of ae list
		| SUM of ae list

let rec diff (ae, str) =
	
	let rec diff_times orihd li s n =
		match li with
		| (hd::tl) -> 
			if hd = orihd && n = 1 then CONST 0
			else SUM (List.append [TIMES ((diff (hd, s))::tl)] [diff_times orihd (List.append tl [hd]) s 1])
	in

	match ae with
	| CONST _ -> CONST 0
	| VAR s ->
		if s = str then CONST 1
		else CONST 0
	| POWER (s, i) ->
		if s = str then TIMES [CONST i; POWER (s, i-1)]
		else CONST 0
	| TIMES l -> 
		if l = [] then raise (Error "no times!")
		else diff_times (List.hd l) l str 0
	| SUM (hd::tl) -> 
		if tl = [] then diff (hd, str)
		else SUM (List.append [diff(hd, str)] [diff (SUM tl, str)]) 
