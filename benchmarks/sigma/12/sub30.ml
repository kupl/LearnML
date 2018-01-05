exception BIG of string

let rec sigma (a, b, f) =
	if a = b then (f a)
	else if a > b then raise (BIG "a is bigger than b")
	else (f a) + (sigma ((a+1), b, f))
;;
