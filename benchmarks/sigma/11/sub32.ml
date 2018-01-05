(* 컴퓨터공학부 2009-11833 창배성 *)
let rec sum (a, b, f) =
	if a>b then raise (Invalid_argument "sum")
	else if a = b then f a
	else (sum (a+1, b, f) + (f a))