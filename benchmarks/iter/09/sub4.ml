exception Error of string;;

let rec iter n f =
	if n < 0 then raise (Error "the number of iteration should be non-negative!")
	else
		match n with
			0 -> (fun a -> a)
		| 1 -> (fun x -> (f x))
		| _ -> (fun x -> ((iter (n - 1) f) (f x)))
;;