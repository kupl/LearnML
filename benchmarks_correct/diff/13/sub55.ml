type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list

exception InvalidArgument

let rec diff : ae * string -> ae = fun (ex, v) -> match (ex, v) with
| (CONST a,_) -> CONST 0
| (VAR s, v) -> (if (s <> v) then CONST 0 else CONST 1)
| (POWER (s, e), v) -> (if (s <> v) then CONST 0 else 
		(if (e = 0) then CONST 0 
		else (TIMES [CONST e; POWER (s, e-1)])))
| (TIMES a, v) -> (match a with 
		| [] -> raise InvalidArgument
		| [single] -> diff (single, v)
		| _ ->  SUM [TIMES (diff (List.hd a, v)::List.tl a) ; (TIMES [(List.hd a);(diff (TIMES (List.tl a), v))])] 
		)
| (SUM a, v) -> (match a with
		| [] -> raise InvalidArgument
		| [single] -> diff (single, v)
		| _ -> SUM [diff (List.hd a, v); diff (SUM (List.tl a), v)]
		)

