(* Exercise 1 *)
exception Error of string

let rec sigma (a, b, f) =
	match (a - b) with
		0 ->
			(f a)
		| _ ->
			(if (a > b) then
				raise (Error "Invalid arguments : a is larger than b")
			else
				((f a) + (sigma ((a + 1), b, f))))
