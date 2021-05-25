(* hw1-3 *)
(* 2010-11687 Keunjun Choi *)

exception ERROR of string
let rec iter (n, f) =
	if n<0 then raise (ERROR "n>=0")
	else if n=0 then fun x->x
	else fun x->f (iter (n-1, f) x)

