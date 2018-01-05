type ae = CONST of int
	|VAR of string
	|POWER of string * int
	|TIMES of ae list
	|SUM of ae list

let rec diff (exp ,s) = 
	let rec mul (a, b) = 
		match b with
		| [] -> [];
		| hd::tl ->  (TIMES ( (diff (hd,s))::(List.append a tl)) ) ::mul(hd::a, tl)
	in
	let rec hap a = 
		match a with
		| [] -> [];
		| hd::tl -> (diff (hd, s))::(hap tl)
	in
	match exp with
	| CONST a -> CONST 0
	| VAR a -> if a = s then CONST 1 else CONST 0
	| POWER (a, b) -> if a = s then TIMES [CONST b; POWER (a, b-1)] else CONST 0
	| TIMES a -> SUM (mul ([], a))
	| SUM a -> SUM (hap a)
