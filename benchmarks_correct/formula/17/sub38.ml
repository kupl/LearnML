type formula = TRUE
        | FALSE
        | NOT of formula
        | ANDALSO of formula * formula
        | ORELSE of formula * formula
        | IMPLY of formula * formula
        | LESS of expr * expr

and expr = NUM of int
        | PLUS of expr * expr
        | MINUS of expr * expr

let rec calc a =
        match a with
        | NUM a -> a
        | PLUS (a, b) -> (calc a)+(calc b)
        | MINUS (a, b) -> (calc a)-(calc b)

let rec eval form =
	match form with
	| TRUE -> true
	| FALSE	-> false
	| NOT a -> if (eval a) = true then false else true
	| ANDALSO (a, b) -> if(eval a && eval b) = true then true else false
	| ORELSE (a, b) -> if(eval a || eval b) = true then true else false
	| IMPLY (a, b) -> if(eval a && not(eval b)) = true then false else true
	| LESS (a, b) -> if(calc a < calc b) = true then true else false

