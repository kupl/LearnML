
let rec sigma ((a : int), (b : int), (f : int -> int)) : int =
	match compare a b with
	| 1 -> 0
	| _ -> (f a) + sigma ((a + 1), b, f)