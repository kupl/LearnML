type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list

exception InvalidArgument

let rec diff(expr, var) = 
	match expr with
	| CONST(x) -> CONST(0)
	| VAR(x) ->(
		if(x = var) then CONST(1)
		else CONST(0)
	)
	| POWER(x, pow) ->(
		if(x = var) then 
			if(pow != 0) then TIMES([CONST(pow); POWER(x, pow - 1)])
			else CONST(0)
		else CONST(0)
	)
	| TIMES(xlist) -> (
		if(xlist = []) then raise InvalidArgument
		else if(List.length xlist = 1) then diff(List.hd xlist, var)
		else SUM([TIMES([diff(List.hd xlist, var); TIMES(List.tl xlist)]); TIMES([List.hd xlist; diff(TIMES(List.tl xlist), var)])])
	)
	| SUM(xlist) -> (
		if(xlist = []) then raise InvalidArgument
		else if(List.length xlist = 1) then diff(List.hd xlist, var)
		else SUM([diff(List.hd xlist, var); diff(SUM(List.tl xlist), var)])
	)

