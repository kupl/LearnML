type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
type formula = TRUE
| FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr

let rec eval form =
	let rec evalu e =
		match e with
	 	NUM(x)->x
		|PLUS(x,y)->(evalu x) + (evalu y)
		|MINUS(x,y)->(evalu x) - (evalu y)
		in
	match form with
	FALSE->false
	|TRUE->true
	|NOT(x)->not(eval x)
	|ANDALSO(x,y)->(eval x)&&(eval y)
	|ORELSE(x,y)->(eval x)||(eval y)
	|IMPLY(x,y)->not(eval x)||(eval y)
	|LESS(x,y)-> (evalu x) < (evalu y)

