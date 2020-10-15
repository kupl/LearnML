exception Error of string
let rec sigma f a b =
	if b = a then (f a)
	else if b < a then raise (Error "a is bigger than b.")
	else (f b) + sigma f a (b-1);;
