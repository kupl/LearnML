exception Error of string

let rec sigma f a b = 
	if b < a then raise (Error "error")
	else if b = a then f a
	else f a + sigma f (a+1) b

