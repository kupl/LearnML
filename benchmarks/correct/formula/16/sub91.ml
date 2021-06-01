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
= fun f -> true (* TODO *)

let rec eval : formula -> bool
= fun f -> match f with
True -> true
| False -> false
| Not(a) -> if eval a = true then false else true
| AndAlso(a, b) -> if eval a = true && eval b = true then true else false
| OrElse(a, b) -> if eval a = true || eval b = true then true else false
| Imply(a, b) -> if eval a = false || eval b = true then true else false
| Equal(a, b) ->
let rec res p = match p with
| Plus(x, y) -> res(x) + res(y)
| Minus(x, y) -> res(x) - res(y)
| Num x -> x
in if res(a) = res(b) then true else false
