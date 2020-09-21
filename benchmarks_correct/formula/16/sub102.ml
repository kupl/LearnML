type formula =
	| True
	| False
	| Not of formula 
	| AndAlso of formula * formula 
	| OrElse of formula * formula 
	| Imply of formula * formula 
	| Equal of exp * exp

and exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 

let rec cal_exp n =
match n with
| Num a -> a
| Plus (e1, e2) -> (cal_exp e1 + cal_exp e2)
| Minus (e1, e2) -> (cal_exp e1 - cal_exp e2)

let rec eval : formula -> bool
= fun f -> match f with
| True -> true
| False -> false
| Not x -> 
	let v = eval x in
	(match v with
	| true -> eval False
	| false -> eval True)
| AndAlso (e1,e2) ->
	let v1 = eval e1 in
	let v2 = eval e2 in
		(match v1,v2 with
		| true,true -> eval True
		| true,false -> eval False
		| false,true -> eval False
		| false,false -> eval False)
| OrElse (e1,e2) ->
	let v1 = eval e1 in
	let v2 = eval e2 in
		(match v1,v2 with
		| true,true -> eval True
		| true,false -> eval True
		| false,true -> eval True
		| false,false -> eval False)
| Imply (e1,e2) ->
	let v1 = eval e1 in
	let v2 = eval e2 in
		(match v1,v2 with
		| true,true -> eval True
		| true,false -> eval False
		| false,true -> eval True
		| false,false -> eval True)
| Equal (e1,e2) -> 
	let v1 = cal_exp e1 in
	let v2 = cal_exp e2 in
	(match v1,v2 with
	| v1,v2 -> if v1 = v2 then eval True else eval False )