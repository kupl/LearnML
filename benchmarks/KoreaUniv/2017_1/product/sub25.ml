
let rec product : (int -> int) -> int -> int -> int = fun f a b ->
	if a>b then raise (Failure "a is larger then b.")
	else if a = b then f b
	else (f a) * (product f (a+1) b);;
