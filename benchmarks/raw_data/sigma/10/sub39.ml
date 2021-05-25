exception Error of string
let rec sigma (a, b, f) =
	if b = a then (f a)
	else if b < a then raise (Error "a is bigger than b.")
	else (f b) + sigma (a, (b-1), f);;
