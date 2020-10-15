exception Error of string

let rec sigma f a b =
	if (a>b) then raise (Error "Invalid argument")
	else if (a=b) then f b
		else sigma f (a+1) b +(f a)