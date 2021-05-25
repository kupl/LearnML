(*problem 3*)
let rec comp (n, f) x =
	if(n = 0) then x
	else comp(n-1, f) (f x) 

let iter : int * (int -> int) -> (int -> int)
= fun (n, f) x ->
	comp(n, f) x