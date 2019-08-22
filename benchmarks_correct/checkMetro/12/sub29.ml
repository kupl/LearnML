type lambda = V of var
		| P of var * lambda
		| C of lambda * lambda
and var = string


let isIn (str, areas) =
	if (List.filter (function a -> a = str) areas) == [] then false else true

let rec subChkMet (met, areas) =
	match met with
	| V n -> if (isIn (n, areas)) then true else false
	| P (n, smet) -> subChkMet(smet, n::areas)
	| C (met1, met2) -> if ((subChkMet (met1, areas))&&(subChkMet (met2, areas))) then true else false

let check met =
        subChkMet (met, [])
