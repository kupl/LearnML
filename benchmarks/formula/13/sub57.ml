type formula = TRUE
	|FALSE
	|NOT of formula
	|ANDALSO of formula*formula
	|ORELSE of formula*formula
	|IMPLY of formula*formula
	|LESS of expr*expr
and expr= NUM of int
	|PLUS of expr*expr
	|MINUS of expr*expr

let rec getexpr inpt =
        match inpt with
        | NUM(form) -> form
        | PLUS(form, lat) -> getexpr form + getexpr lat
        | MINUS(form, lat) -> getexpr form - getexpr lat

let rec eval input = 
	match input  with
	| TRUE -> true
	| FALSE -> false
	| NOT(inpt) -> eval inpt
	| ANDALSO(form, lat) -> eval form && eval lat
	| ORELSE(form, lat) -> eval form || eval lat
	| IMPLY(form, lat) -> not (eval form) || eval lat
	| LESS(form , lat) -> getexpr form < getexpr lat
