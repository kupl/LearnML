(* Ex 2 *)
type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

exception ILLEGAL_EXPRESSION of string 
let rec diff((ae:ae), (str:string)) = match ae with 
				    | CONST(_) -> CONST(0)
				    | VAR(x) -> if x = str 
						then CONST(1)
						else CONST(0)
				    | POWER(base, expnt) -> if base = str 
							    then TIMES( [CONST(expnt) ; POWER(base, expnt - 1)] )
							    else CONST(0)
				    | TIMES([]) -> raise(ILLEGAL_EXPRESSION "The argument of TIMES is illegal")
				    | TIMES(hd::[]) -> diff(hd, str)
				    | TIMES(hd::tl) -> SUM([TIMES( [diff(hd, str)]@tl )]@[TIMES(hd::[diff(TIMES(tl), str)])])
				    | SUM([]) -> raise(ILLEGAL_EXPRESSION "The argument of SUM is illega")
				    | SUM(hd::[]) -> diff(hd, str)
				    | SUM(hd::tl) -> SUM( [diff(hd, str)] @ [diff(SUM(tl), str)] )