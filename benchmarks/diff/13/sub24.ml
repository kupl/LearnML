type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

exception InvalidArgument

let rec diff (ae, s) = 
	let rec diff_times s l r = 
		match r with
		| hd :: tl -> [TIMES (l @ [diff (hd, s)] @ tl)] @ (diff_times s (l @ [hd]) tl)
		| [] -> []
	in
	let rec diff_sub s ae = 
		match ae with
		| CONST _ -> CONST 0
		| VAR x -> if x = s then CONST 1 else CONST 0
		| POWER (x, n) -> if x <> s then CONST 0 else if n = 0 then CONST 0 else if n = 1 then CONST 1 else TIMES [CONST n; POWER (x, n - 1)]
		| SUM l -> if l = [] then raise InvalidArgument else SUM (List.map (diff_sub s) l)
		| TIMES l -> if l = [] then raise InvalidArgument else SUM (diff_times s [] l)
	in diff_sub s ae
