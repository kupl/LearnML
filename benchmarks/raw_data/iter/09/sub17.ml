exception Error of string

let rec iter (n, f) r = 
	if n = 0 then r
	else if n > 0 then iter (n-1, f) (f r)
	else raise (Error "invalid n")