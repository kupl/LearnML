type ae = CONST of int
				| VAR of string
				| POWER of string * int
				| TIMES of ae list
				| SUM of ae list

exception InvalidArgument

let rec diff (ae, v) =
	let rec aux (ae, v) =
		match ae with
		| CONST c -> CONST 0
		| VAR x -> if x = v then CONST 1 else CONST 0
		| POWER (x, n) -> if x = v
											then TIMES[ (CONST n); (POWER (x, n - 1)) ]
											else CONST 0
		| TIMES [] -> CONST 0
		| TIMES ((CONST 0)::tl) -> CONST 0
		| TIMES (hd::tl) -> SUM (TIMES (aux (hd, v)::tl)::(TIMES (hd::aux ((TIMES tl),v)::[]))::[])
		| SUM aelist -> SUM (List.map (fun ae -> aux (ae, v)) aelist) in 
	match ae with
	| TIMES [] -> raise InvalidArgument
	| SUM [] -> raise InvalidArgument
	| _ -> aux (ae, v)
