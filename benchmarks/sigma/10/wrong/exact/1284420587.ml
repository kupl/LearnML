exception Error of string

let rec sigma a b f = 
	if a = b then f a
	else if a > b then raise (Error "invalid input")
		else (f a) + sigma (a+1) b f
;;