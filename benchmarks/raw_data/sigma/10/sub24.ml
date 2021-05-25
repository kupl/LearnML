
(*Ex1*)
exception Error of string

let rec sigma(a, b, (f : int -> int)) =
	if (a > b) then raise (Error "a is biger than b")
	else if (a = b) then f(a)
	   		 	    else f(a) + sigma(a+1,b, f)

