exception InvalidInput of string

let rec iter(n, func) =
	if(n<0) then raise (InvalidInput "n is negative")
	else if(n==0) then (function x -> x)
	else (function x -> func (iter(n-1, func) x))
