(* 컴퓨터공학부/2009-11679/김정명/2 *)

let rec iter (n, f) a =
	if n = 0 then a
	else iter (n-1, f) (f a)
