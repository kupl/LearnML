exception Error of string

let rec sigma f a b = 
	if a = b then f (a)
	else if a > b then raise (Error "invalid input")
		else f (a) + sigma f (a+1) b
;;