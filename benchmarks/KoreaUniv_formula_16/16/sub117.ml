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
= fun f -> 
let rec expEval : exp -> int
= fun e ->
match e with
Num(x) -> x
|Plus(a, b) -> expEval a + expEval b
|Minus(a, b) -> expEval a - expEval b in
match f with
True -> true
|False -> false
|Not(x) -> not (eval x)
|AndAlso(a, b) -> (eval a) && (eval b)
|OrElse(a, b) -> (eval a) || (eval b)
|Imply(a, b) -> (not (eval a)) || (eval b)
|Equal(a, b) -> if expEval a = expEval b then true else false;;

