let rec help3 n f r =
	if (n = 0) then
		r
	else
		f (help3 (n-1) f r)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> (fun a -> help3 n f a);;
