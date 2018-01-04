type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

exception InvalidArgument

let rec diff (a, s) =
match a with
| CONST c -> CONST 0
| VAR c -> if c=s then CONST 1
		else CONST 0
| POWER (c, n) -> if c=s then 
			if n!=0 then TIMES (CONST n :: [POWER (c, n-1)])
			else CONST 0
		else CONST 0
| TIMES [] -> raise InvalidArgument
| TIMES (hd::[]) -> diff (hd, s)
| TIMES (hd::tl) -> SUM [TIMES [ (diff (hd, s)) ; TIMES tl] ; (TIMES [ hd ; (diff (TIMES tl, s))])] 
| SUM [] -> raise InvalidArgument
| SUM (hd::[]) -> diff (hd, s)
| SUM (hd::tl) -> SUM [diff (hd, s) ; diff(SUM tl, s)]
