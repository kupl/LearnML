type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

let rec diff (exp, s) =
	match exp with
	| CONST i -> CONST 0
	| VAR a -> if a = s then CONST 1 else CONST 0
	| POWER (a, i) -> if a=s then begin if i=0 then CONST 0
					    else if i=1 then CONST 1
					    else TIMES[CONST i; POWER(a, i-1)]
				      end
			  else CONST 0
	| SUM lst ->
		(match lst with
		| [] -> CONST 0
		| hd::[] -> diff (hd, s)
		| hd::tl -> SUM [diff (hd, s); diff (SUM tl, s)])
	| TIMES lst ->
		match lst with
		| [] -> CONST 0
		| _ -> SUM (tCheck (List.rev lst, (List.length lst)-1, s))

	and partT (lst, n, s) =
		match lst with
		| [] -> []
		| hd::tl -> if n = 0 then (diff (hd, s))::tl else hd::(partT (tl, (n-1),s ))
	and tCheck (lst, n, s) =
		match lst with
		| [] -> []
		| _ ->  if n = 0 then (TIMES (partT (lst , 0, s)))::[]
			else ((TIMES (partT (lst, n, s)))::(tCheck(lst, n-1,s)))
			
(*			match hd with
			| CONST x -> diff(tl, s)
			| VAL x -> diff(hd, s)::diff(tl, s)
			| POWER x -> diff(hd, s)::diff(tl, s)
			| TIMES x -> diff(hd, s)@diff(tl, s)
			| SUM x -> diff(hd, s)@diff(tl, s)
*)
