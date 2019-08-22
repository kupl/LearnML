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
		if List.exists (fun x-> a=x) area_list then true
		else false

let check: lambda -> bool =
	fun(input) ->
	let area_list:var list=[] in
	innerlambda (area_list, input)
	
(*
let rec check: lambda -> bool =
	fun(input) ->
	match input with
	| C (a, b) -> (check a && check b)
	| P (a, b) ->
		if a==[] then check b
		else if		
		if 
		| [] -> check b
		| Lists.exists (fun x->a=x) area_list =true
	| V a -> List.exists (fun x->a=x) area_list
*)
(*
let rec check: lambda->bool =
	fun(input) ->
	if input = C (a,b) then (check a && check b)
	else if input = P (a,b) then 
		if (List.exists (fun x->a=x) area_list) then check b
		else area_list=area_list::a in
*)
