(* hw1-2 *)
(* 2010-11687 Keunjun Choi *)

let rec sigma f a b =
	if a>b then 0
	else if a=b then f a 
	else sigma f (a+1) b+f a	
