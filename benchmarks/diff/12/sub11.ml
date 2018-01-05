type ae = CONST of int
				| VAR of string
				| POWER of string * int
				| TIMES of ae list
				| SUM of ae list

let rec diff ((a:ae), (s:string)) =
	match a with
		| CONST i -> CONST 0
		| VAR v -> if v = s then CONST 1
							 else CONST 0
		| POWER (v, p) -> if v = s then TIMES ((CONST p)::(POWER (v, p-1))::[])
											else CONST 0
		| TIMES l -> SUM (List.map (fun x -> TIMES ((diff (x, s))::(List.filter (fun y -> y != x) l))) l)
		| SUM l -> SUM (List.map (fun x -> diff (x, s)) l)

