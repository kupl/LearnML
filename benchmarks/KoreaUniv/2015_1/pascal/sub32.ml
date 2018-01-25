(* 2012210066 컴퓨터학과  조현상*)

(* problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 1
let rec pascal (x,y) =
	if x=y then 1 else if y=0 then 1 else if y>x then raise(Failure "Error")
	else pascal (x-1,y-1) + pascal (x-1,y)
