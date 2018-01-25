(* problem 3*)
let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> let rec loop n x =
	if n=0 then x
	else loop (n-1) (f x)
	
	in loop n