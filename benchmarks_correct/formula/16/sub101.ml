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

let rec eval : formula -> bool
= fun f ->  (* TODO *)
match f with
| True -> true
| False -> false
| Not f1 ->
let v1 = eval f1 in (if v1 = true then false else true)
| AndAlso (f1, f2) ->
		let v1 = eval f1 in
		let v2 = eval f2 in (v1 && v2)
| OrElse (f1, f2) ->
		let v1 = eval f1 in
		let v2 = eval f2 in (v1 || v2)
| Imply (f1, f2) ->
		let v1 = eval f1 in
		let v2 = eval f2 in
		(match v1, v2 with
			|true,false -> false
			|_ -> true)
|Equal (e1, e2) ->
let rec ev : exp -> int
= fun a -> 
match a with
| Num c -> c
| Plus (c1, c2) -> ((ev c1) + (ev c2))
| Minus (c1, c2) -> ((ev c1) - (ev c2)) in
let v1 = ev e1 in let v2 = ev e2 in
(if v1 = v2 then true else false)



