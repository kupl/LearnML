
type formula = True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp = Num of int
| Plus of exp * exp
| Minus of exp * exp

let rec eval : formula -> bool = 
	let rec exp_to_int : exp -> int = fun exp ->
	match exp with
	| Num a -> a
	| Plus (a, b) -> exp_to_int(a) + exp_to_int(b)
	| Minus (a, b) -> exp_to_int(a) - exp_to_int(b) in
fun formula ->
match formula with
| True -> true
| False -> false
| Not a -> 
	if(eval a) then false
	else true
| AndAlso (a, b) -> 
	if(eval a) then eval b
	else false
| OrElse (a, b) ->
	if(eval a) then true
	else eval b
| Imply (a, b) ->
	if(eval a) then eval b
	else true
| Equal (a, b) -> exp_to_int(a) = exp_to_int(b)


