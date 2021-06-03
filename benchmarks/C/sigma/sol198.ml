(*
	department : computer science & engineering
	student ID : 2012-11242 / name : Seon-bi, Park
*)

let rec sigma f a b =
	if a>b then 0
	else if a=b then (f a)
	else ((f a) + (sigma f (a+1) b))
