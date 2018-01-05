type expr = NUM of int
			|PLUS of expr * expr
			|MINUS of expr * expr

type formula = TRUE
			|FALSE
			|NOT of formula
			|ANDALSO of formula * formula
			|ORELSE of formula * formula
			|IMPLY of formula * formula
			|LESS of expr * expr

let not_formula a = 
	match a with
	|true -> false
	|false -> true

let andalso_formula (a, b) =
	if a = true && b = true then true
	else false

let orelse_formula (a, b) =
	if a = true || b = true then true
	else false

let imply_formula (a, b) =
	orelse_formula ((not_formula a), b)

let less_formula (a, b) = 
	if a - b <0 then true
	else false

let rec calc expr =
	match expr with
	|NUM a -> a
	|PLUS (a, b) -> (calc a) + (calc b)
	|MINUS (a, b) -> (calc a) - (calc b) 

let rec eval formula = 
	match formula with
	|TRUE -> true
	|FALSE -> false
	|NOT a -> not_formula (eval a)
	|ANDALSO (a, b) -> andalso_formula ((eval a), (eval b))
	|ORELSE (a, b) -> orelse_formula ((eval a), (eval b))
	|IMPLY (a, b) -> imply_formula ((eval a), (eval b))
	|LESS (a, b) -> less_formula ((calc a), (calc b))
