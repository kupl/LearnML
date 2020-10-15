(* Exercise 1 *)
exception Error of string

let rec sigma f a b =
	match (a - b) with
		0 ->
			(f a)
		| _ ->
			(if (a > b) then
				raise (Error "Invalid arguments : a is larger than b")
			else
				((f a) + (sigma f (a+1) b)))
