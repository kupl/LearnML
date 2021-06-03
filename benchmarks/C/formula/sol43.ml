type exp = Num of int
| Plus of exp * exp
| Minus of exp * exp
type formula = True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp

let rec eval form =
	let rec evalu e =
		match e with
	 	Num(x)->x
		|Plus(x,y)->(evalu x) + (evalu y)
		|Minus(x,y)->(evalu x) - (evalu y)
		in
	match form with
	False->false
	|True->true
	|Not(x)->not(eval x)
	|AndAlso(x,y)->(eval x)&&(eval y)
	|OrElse(x,y)->(eval x)||(eval y)
	|Imply(x,y)->not(eval x)||(eval y)
	|Equal(x,y)-> (evalu x) = (evalu y)

