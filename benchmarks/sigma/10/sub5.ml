(* CSE/ 2004-11920 / Yeseong Kim/ Prob 1*)
exception Error of string

let rec sigma (a, b, f)  =
	if (a > b) then raise (Error "A > B")
	else if (a = b) then f(a)
	else f(a) + sigma(a+1, b, f)
