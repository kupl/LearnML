type lambda = 
  V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec checkArea (e, m) =
	match m with
	| V s ->
		(match e with
		| [] -> false
		| h::[] -> if(s = h) then true else false
		| h::t -> if(s = h) then true else checkArea(t, V s))
	| C (m1, m2) ->
		if (checkArea (e, m1) && checkArea(e, m2)) then true else false
	| P (a, m) -> checkArea (a::e, m)

let check m = checkArea ([], m)

