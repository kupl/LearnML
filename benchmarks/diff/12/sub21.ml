type ae =
	  CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

let rec diff (f, x) =
	let rec t_diff (i, n, l) = match l with
		| h::rm ->
			if i = n then TIMES (List.append [diff (h,x)] rm)
			else SUM [TIMES (List.append [diff (h, x)] rm); t_diff (i + 1, n, List.append rm [h])]
	in
	match f with
		| CONST i -> CONST 0
		| VAR v ->
			if v = x then CONST 1
			else CONST 0
		| POWER (v, n) ->
			if n = 0 || v != x then CONST 0
			else TIMES [CONST n; POWER (v, n - 1)]
		| TIMES l ->
			if (List.length l) = 0 then invalid_arg "Empty TIMES"
			else if (List.length l) = 1 then diff ((List.hd l), x)
			else t_diff (1, List.length l, l)
		| SUM l -> (match l with
			| [] -> CONST 0
			| [a] -> diff (a, x)
			| h::rm -> SUM [diff (h,x); diff (SUM rm, x)])