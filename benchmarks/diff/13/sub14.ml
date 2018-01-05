type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

exception InvalidArgument

let diff (a, b) = 
	let rec realdiff (a, b) = 
		match a with
		| CONST x -> CONST 0
		| VAR x -> if x=b then CONST 1
			   else CONST 0
		| POWER (x, y) -> if x=b then TIMES [CONST y;POWER (x, y-1)]
				  else CONST 0
		| TIMES lst -> (match lst with
				| hd::[] -> realdiff (hd, b)
				| hd::tl -> SUM [TIMES [hd;realdiff(TIMES tl, b)] ; TIMES [realdiff(hd, b);TIMES tl]]
				| [] -> raise InvalidArgument
			       )
		| SUM lst -> (match lst with
				| hd::[] -> realdiff (hd, b)
				| hd::tl -> SUM[realdiff(hd, b);realdiff(SUM tl, b)]
				| [] -> raise InvalidArgument
			     )
	in
	let maketime (a, b) = 
		match b with
		| TIMES lst -> TIMES (a::lst)
		| _ -> TIMES (a::b::[])
	in
	let makesum (a, b) = 
		match b with
		| SUM lst -> SUM (a::lst)
		| _ -> SUM (a::b::[])
	in
	let rec simplifier a = 
		match a with
		| CONST x -> CONST x
		| VAR x -> VAR x
		| POWER (x, y) -> POWER (x, y)
		| TIMES lst -> (match lst with
				| hd::[] -> simplifier hd
				| hd::tl -> if (simplifier (TIMES tl))=(CONST 1) then simplifier hd
					    else if (simplifier (TIMES tl))=(CONST 0) then CONST 0
					    else if (simplifier hd)=(CONST 0) then CONST 0
					    else if (simplifier hd)=(CONST 1) then simplifier (TIMES tl)
					    else maketime (simplifier hd, simplifier (TIMES tl))
				| [] -> raise InvalidArgument
				)
		| SUM lst -> (match lst with
				| hd::[] -> simplifier hd
				| hd::tl -> if (simplifier (SUM tl))=(CONST 0) then simplifier hd
					    else if (simplifier hd)=(CONST 0) then simplifier (SUM tl)
					    else makesum (simplifier hd, simplifier (SUM tl))
				| [] -> raise InvalidArgument
			     )
	in
	simplifier (realdiff (a, b))
		   
