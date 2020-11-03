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

let rec e2i e =
	match e with
	| Num (n) -> n
	| Plus (e1, e2) -> (e2i e1) + (e2i e2)
	| Minus (e3, e4) -> (e2i e3) - (e2i e4) 

let rec eval : formula -> bool
= fun f ->
	match f with
	| True -> true
	| False -> false
	| Not (f1) -> if (eval f1) = true then false else true
	| AndAlso (f2, f3) -> (eval f2)&&(eval f3)
	| OrElse (f4, f5) -> (eval f4)||(eval f5)
	| Imply (f6, f7) ->
		begin
		match (eval f6,eval f7) with
		| (true, false) -> false
		| _ -> true
		end
	| Equal (e1,e2) -> 
		let v1 = (e2i e1) in
		let v2 = (e2i e2) in if v1 = v2 then true else false
