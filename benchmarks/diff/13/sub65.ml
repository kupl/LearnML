type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list
exception InvalidArgument
let rec diff(ae,s) =
	match ae with
	|CONST n -> CONST(0)
	|VAR x -> if (compare s x) == 0 then CONST(1)
              else CONST(0)
	|POWER(x,n) -> if (compare s x) == 0 then (
					   if n == 0 then CONST(0)
					   else if n == 1 then CONST(1)
					   else TIMES([CONST(n);POWER(x,n-1)])
				   )
				else POWER(x,n)
	|TIMES(ael) ->  if (List.length ael) == 0 then raise InvalidArgument
					else if (List.length ael) == 1 then diff((List.hd ael),s)
					else SUM([TIMES(diff((List.hd ael),s)::(List.tl ael));TIMES([(List.hd ael);diff(TIMES(List.tl ael),s)])])
	|SUM(ael) -> if (List.length ael) == 0 then raise InvalidArgument
				 else if (List.length ael) == 1 then diff((List.hd ael),s)
				 else SUM([diff((List.hd ael),s);diff(SUM(List.tl ael),s)])