
(* Exercise 2*)
let rec sigma f1 a b =
	if a > b then 0
	else if a == b then f1 b
	else (f1 a) + (sigma f1 (a+1) b) 
