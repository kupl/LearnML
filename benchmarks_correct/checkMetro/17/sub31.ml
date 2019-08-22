type lambda = V of var
		   | P of var * lambda
		   | C of lambda * lambda
  and var = string

let check : lambda -> bool = fun m ->
	let rec checklambda' = fun m varlist ->
		match m with
		| V var -> if (List.mem var varlist) then true
						  else false
		| P (var, lambda) ->	checklambda' lambda (var::varlist)
		| C (met1, met2) -> checklambda' met1 varlist && checklambda' met2 varlist
	in
	checklambda' m []


