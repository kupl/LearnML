exception Error of string

let rec sigma (a, b, f) = 
	if b < a then raise (Error "error")
	else if b = a then f a
	else f a + sigma (a+1, b, f)

