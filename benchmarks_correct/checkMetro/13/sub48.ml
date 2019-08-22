type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let rec list_del lst element =
	match lst with
	| [] -> []
	| head :: tail -> if head = element then list_del tail element
			  else head :: (list_del tail element)

let rec remainStation m =
	match m with
	| V str -> [str]
	| P (str, met) -> list_del (remainStation met) str
	| C (met1, met2) -> (remainStation met1) @ (remainStation met2)

let rec check m =
	if (List.length (remainStation m)) = 0 then true
	else false

