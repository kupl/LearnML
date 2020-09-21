type formula =
	|True
	|False
	|Not of formula
	|AndAlso of formula * formula
	|OrElse of formula * formula
	|Imply of formula * formula
	|Equal of exp * exp
and exp =
	|Num of int
	|Plus of exp * exp
	|Minus of exp * exp;;

let rec exp_eval e = match e with
	|Num a -> a
	|Plus (a, b) -> exp_eval a + exp_eval b
	|Minus (a, b) -> exp_eval a - exp_eval b;;

let rec eval s = match s with
	|True -> true
	|False -> false
	|Not f -> if eval f then false else true
	|AndAlso (a, b) -> if eval a && eval b then true  else false
	|OrElse (a, b) -> if eval a || eval b then true else false
	|Imply (a, b) -> if eval a && eval b == false then false else true
	|Equal (a,b) -> if exp_eval a == exp_eval b then true else false;;
       