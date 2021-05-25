(* hw1-2 *)
(* 2010-11687 Keunjun Choi *)

let rec sigma (a, b, f) =
	if a>b then 0
	else if a=b then f a 
	else sigma (a+1, b, f)+f a	
