type lambda = V of var
	| P of var * lambda
	| C of lambda*lambda
and var=string

let rec innerlambda: var list * lambda -> bool =
	fun(area_list, input) ->
	match input with
	| C (a,b) -> innerlambda(area_list, a) && innerlambda(area_list, b)
	| P (a,b) -> 
		if (List.exists (fun x-> a=x) area_list) then innerlambda(area_list, b)
		else innerlambda(a::area_list, b)
	| V a ->
		if (List.exists (fun x-> a=x) area_list) then true
		else false

let check: lambda -> bool =
	fun(input) ->
	let area_list:var list=[] in
	innerlambda (area_list, input)
