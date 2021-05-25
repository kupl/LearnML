exception Error of string

let rec sigma (a, b, f) =
	if a > b 
		then raise (Error "Fuck You!")
	else 
		if a = b then f b
		else (f a)+(sigma((a+1), b, f));;
