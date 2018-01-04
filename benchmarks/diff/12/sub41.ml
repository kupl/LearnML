type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

let rec diff (ae,str) = 
	match ae with
	|CONST n -> CONST 0
	|VAR s -> if (s=str) then CONST 1
		  else CONST 0
	|POWER (s,i) -> if (s=str) then TIMES[CONST i; POWER(s,i-1)]
			else CONST 0
	|TIMES (hd::[]) -> diff(hd,str)
	|TIMES (hd::tl) -> SUM[TIMES[(diff (hd,str));TIMES tl];TIMES[hd;(diff (TIMES tl,str))]]
	|SUM (hd::[]) -> diff(hd,str)
	|SUM (hd::tl) -> SUM[(diff (hd,str));(diff (SUM tl,str))]
