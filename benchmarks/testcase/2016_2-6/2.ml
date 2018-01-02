type exp = 
	|Num of int
	|Plus of exp * exp
	|Minus of exp * exp

type formula = 
	| True
	| False
	| Not of formula
	| AndAlso of formula * formula
	| OrElse of formula * formula
	| Imply of formula * formula 
	| Equal of exp * exp

let rec f form =
match form with
| True -> true 
| False -> false
| Not a -> if f a then false else true
| AndAlso (left, right) -> if f left && f right then true else false
| OrElse (left, right) -> if f left||f right then true else false
| Imply (left, right) -> if f left && f right then true else if f left=true && f right=false then true else false
| Equal (left, right) ->
	let rec env v = match v with
	|Num (a) -> a
	|Plus (a, b) -> env (a) + env (b)
	|Minus (a, b) -> env (a) - env (b)
in if (env (left) = env (right)) then true else false   ;;