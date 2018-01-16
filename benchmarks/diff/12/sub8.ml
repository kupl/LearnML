
exception Error of string

type ae = CONST of int
		| VAR of string
		| POWER of string * int
		| TIMES of ae list
		| SUM of ae list

let rec diff (ae_exp, str_diff) =
	(
	match ae_exp with
	| CONST (i) -> CONST 0
	| VAR (str) ->	if str=str_diff then
						CONST 1
					else
						CONST 0
	| POWER (str, i) -> if str=str_diff then
						TIMES [CONST i;POWER (str, i-1)]
					else
						CONST 0
	| TIMES (list_ae) ->
		(
		match list_ae with
		| [] -> raise (Error "Invalid TIMES []")
		| hd::[] -> diff(hd, str_diff)
		| hd::tl -> SUM [TIMES (diff(hd, str_diff)::tl);TIMES (hd::[diff(TIMES tl, str_diff)])]
		)
	| SUM (list_ae) ->
		(
		match list_ae with
		| [] -> raise (Error "Invalid SUM []")
		| _ ->
			let rec diff_sum (list_ae_, result) =
				(
				match list_ae_ with
				| [] -> result
				| hd::tl -> diff_sum(tl, List.append result [diff(hd, str_diff)])
				)
			in
			SUM (diff_sum (list_ae, []))
		)
	)


