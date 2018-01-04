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
		| POWER (v, p) -> if v <> s || p = 0 then CONST 0
											else if p = 1 then CONST 1
											else TIMES ((CONST p)::(POWER (v, p-1))::[])
											
		| TIMES l -> SUM (List.map (fun x -> if (diff (x, s) = CONST 0) then CONST 0
																				 else if (diff(x, s) = CONST 1) then TIMES (List.filter (fun y -> y != x) l)
																				 else TIMES ((diff (x, s))::(List.filter (fun y -> y != x) l))) l)
		| SUM l -> SUM (List.filter (fun x -> x <> CONST 0) (List.map (fun x -> diff (x, s)) l))

