exception Error of string

let rec sigma (a,b,f) =
	if (a>b) then raise (Error "Invalid argument")
	else if (a=b) then f b
		else sigma(a+1,b,f)+(f a)