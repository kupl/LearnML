(*
	department : computer science & engineering
	student ID : 2012-11242 / name : Seon-bi, Park
*)

let rec sigma (a,b,f) =		(*  int * int * (int -> int) -> int  *)
	if a>b then 0
	else if a=b then (f a)
	else ((f a) + (sigma (a+1, b, f)))
