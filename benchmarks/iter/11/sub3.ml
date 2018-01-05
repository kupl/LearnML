let rec iter (n, f) =
	match n with
	| 0 	->	fun a -> a
	| _ 	->	fun a -> iter(n-1,f) (f a)