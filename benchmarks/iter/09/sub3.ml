let rec iter n f =
	match n with
	  0 -> (fun a -> a)
	| 1 -> (fun x -> (f x))
	| _ -> (fun x -> ((iter (n - 1) f) (f x)))
;;