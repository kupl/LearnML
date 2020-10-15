exception Error of string

let rec sigma func a b =
	if (a < b) then
	(func a) + sigma func (a+1) b
	else if (a = b) then
	func b
	else
	raise (Error "invalid")
