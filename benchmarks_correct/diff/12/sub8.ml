
exception Error of string

type aexp = Const of int
		| Var of string
		| Power of string * int
		| Times of aexp list
		| Sum of aexp list

let rec diff (aexp_exp, str_diff) =
	(
	match aexp_exp with
	| Const (i) -> Const 0
	| Var (str) ->	if str=str_diff then
						Const 1
					else
						Const 0
	| Power (str, i) -> if str=str_diff then
						Times [Const i;Power (str, i-1)]
					else
						Const 0
	| Times (list_aexp) ->
		(
		match list_aexp with
		| [] -> raise (Error "Invalid Times []")
		| hd::[] -> diff(hd, str_diff)
		| hd::tl -> Sum [Times (diff(hd, str_diff)::tl);Times (hd::[diff(Times tl, str_diff)])]
		)
	| Sum (list_aexp) ->
		(
		match list_aexp with
		| [] -> raise (Error "Invalid Sum []")
		| _ ->
			let rec diff_sum (list_aexp_, result) =
				(
				match list_aexp_ with
				| [] -> result
				| hd::tl -> diff_sum(tl, List.append result [diff(hd, str_diff)])
				)
			in
			Sum (diff_sum (list_aexp, []))
		)
	)


