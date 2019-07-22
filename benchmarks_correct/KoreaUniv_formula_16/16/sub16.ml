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

let rec exptoint : exp -> int
= fun e ->
	match e with
	| Num(a) -> a
	| Plus(a,b) -> exptoint a + exptoint b
	| Minus(a,b) -> exptoint a - exptoint b

let rec eval : formula -> bool
= fun f ->  (* TODO *)
	match f with
	| True -> true
	| False -> false
 	| Not(a) -> 
			if eval a = true then false
			else true
	| AndAlso(a,b) ->
			if eval a = true && eval b = true then true
			else false
	| OrElse(a,b) ->
			if eval a = true || eval b = true then true
			else false
	| Imply(a,b) ->
			if eval a = true && eval b = false then false
			else true
	| Equal(a,b) ->
			if exptoint a = exptoint b then true
			else false
