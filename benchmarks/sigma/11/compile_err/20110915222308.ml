(* ��ǻ�Ͱ��к� 2009-11833 â�輺 *)
let rec sum (a, b, f) =
	if a>b then raise (Invalid_argument "sum")
	else if a = b then f a
	else (sum (a+1, b, f) + (f a))