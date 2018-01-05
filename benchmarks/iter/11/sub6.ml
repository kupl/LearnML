

let rec iter(n, f) x =
	if n=0 then x
	else iter(n-1, f)(f x)
(*
let _= 
	print_newline();
	print_int(iter(4, fun x->2+x ) 0)
*)
