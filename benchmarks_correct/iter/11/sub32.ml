(* 컴퓨터공학부 2009-11833 창배성 *)
let rec iter (n, f) a =
	if n<0 then raise (Invalid_argument "iter")
	else if n = 0 then a
	else if n = 1 then f a
	else f (iter(n-1, f) a)