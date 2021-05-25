exception InvalidRange

let rec sigma(a,b,f) =
	if a == b then f a
	else if a>b then raise InvalidRange
	else (f a) + sigma(a+1,b,f)

