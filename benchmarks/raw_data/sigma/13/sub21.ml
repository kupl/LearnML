(*2009-11718 1-1*)

let rec sigma (a, b, f) =
	if a>b then 0
	else if a=b then (f a)
	else (f a) + sigma (a+1, b, f)

