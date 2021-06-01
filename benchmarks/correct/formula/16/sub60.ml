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
= fun f -> match f with
	| True -> true
	| False -> false
	| Not v ->
		let e =  eval v in
			 if (e=true) then false else true
	| AndAlso (v1,v2) ->
		let e1 = eval v1 in
		let e2 = eval v2 in
			begin
			 if(e1=true&&e2=true) then true else false
			end
	|OrElse (v1,v2) ->
		let e1 = eval v1 in
		let e2 = eval v2 in
			begin
			 if(e1=false&&e2=false) then false else true
			end
	|Imply (v1,v2) ->
		let e1 = eval v1 in
		let e2 = eval v2 in
			begin
			 if(e1=true&&e2=false) then false else true
			end
	|Equal (v1,v2) ->
		let rec eval2 : exp -> int
		=fun g -> match g with
			| Num s -> s
			| Plus (s1,s2) ->
				 (eval2 s1)+(eval2 s2)
			| Minus (s1,s2) ->
				 (eval2 s1)-(eval2 s2) in
		let e1 = eval2 v1 in
		let e2 = eval2 v2 in
			begin
			 if(e1=e2) then true else false
			end


