(* CSE/ 2004-11920 / Yeseong Kim/ Prob 1*)
exception Error of string

let rec sigma f a b =
	if (a > b) then raise (Error "A > B")
	else if (a = b) then f(a)
	else f(a) + sigma f (a+1) b
