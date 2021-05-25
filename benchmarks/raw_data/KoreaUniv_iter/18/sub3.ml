let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 
	let rec func : (int -> int) -> int -> (int -> int) = fun f n ->
		if n = 0 then (fun x -> x)
		else (fun x -> (func f (n - 1)) (f x)) in func f n;;