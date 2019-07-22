type ae = 
	  CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

let rec diff (exp, var) =
	match exp with
	| CONST(_) -> CONST 0
	| VAR(str) -> if str = var then CONST 1 else CONST 0
	| POWER(str, n) -> if str = var then TIMES( (CONST n) :: (POWER(str, n-1)) :: []) else CONST 0
	| TIMES([]) | SUM([]) -> CONST 0
	| TIMES(hd::tl) -> SUM(
						(TIMES( (diff(hd, var))::tl )) :: 
						(TIMES( hd :: diff(TIMES(tl), var) :: [])) :: [])
	| SUM(hd::tl) -> SUM( (diff(hd, var)) :: (diff(SUM(tl), var)) :: [])
	
