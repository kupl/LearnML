exception Error of string

let rec sigma (a, b, func) =
	if (a < b) then
	(func a) + sigma ((a+1), b, func)
	else if (a = b) then
	func b
	else
	raise (Error "invalid")
