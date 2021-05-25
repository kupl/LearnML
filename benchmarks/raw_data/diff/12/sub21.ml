type aexp =
	  Const of int
	| Var of string
	| Power of string * int
	| Times of aexp list
	| Sum of aexp list

let rec diff (f, x) =
	let rec t_diff (i, n, l) = match l with
		| h::rm ->
			if i = n then Times (List.append [diff (h,x)] rm)
			else Sum [Times (List.append [diff (h, x)] rm); t_diff (i + 1, n, List.append rm [h])]
	in
	match f with
		| Const i -> Const 0
		| Var v ->
			if v = x then Const 1
			else Const 0
		| Power (v, n) ->
			if n = 0 || v != x then Const 0
			else Times [Const n; Power (v, n - 1)]
		| Times l ->
			if (List.length l) = 0 then invalid_arg "Empty Times"
			else if (List.length l) = 1 then diff ((List.hd l), x)
			else t_diff (1, List.length l, l)
		| Sum l -> (match l with
			| [] -> Const 0
			| [a] -> diff (a, x)
			| h::rm -> Sum [diff (h,x); diff (Sum rm, x)])