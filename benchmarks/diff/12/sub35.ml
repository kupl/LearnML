type ae = CONST of int
		| VAR of string
		| POWER of string * int
		| TIMES of ae list
		| SUM of ae list


let rec diff(exp, str) = 
	match exp with 
	| CONST num -> CONST 0
	| VAR st -> 
		if st = str then CONST 1
		else CONST 0
	| POWER(st, num) -> 
		if st = str then 
			if num = 1 then CONST num
			else TIMES(POWER(st, num - 1)::(CONST num)::[])
		else CONST 0
	| SUM a -> SUM(diffSum(a, str))
	| TIMES a -> 
		match a with
		| [] -> CONST 0
		| b::[] -> diff(b, str)
		| b::bl -> SUM(
						TIMES(diff(b, str)::bl)::
TIMES(b::diff(TIMES(bl), str)::[])::[])
and diffSum(explist, str) = 
	match explist with
	| [] -> []
	| a::[] -> diff(a, str)::[]
	| a::al -> diff(a, str)::diffSum(al, str)
