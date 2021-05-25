type exp = Num of int
			|Plus of exp * exp
			|Minus of exp * exp

type formula = True
			|False
			|Not of formula
			|AndAlso of formula * formula
			|OrElse of formula * formula
			|Imply of formula * formula
			|Equal of exp * exp

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
	if a - b =0 then true
	else false

let rec calc exp =
	match exp with
	|Num a -> a
	|Plus (a, b) -> (calc a) + (calc b)
	|Minus (a, b) -> (calc a) - (calc b) 

let rec eval formula = 
	match formula with
	|True -> true
	|False -> false
	|Not a -> not_formula (eval a)
	|AndAlso (a, b) -> andalso_formula ((eval a), (eval b))
	|OrElse (a, b) -> orelse_formula ((eval a), (eval b))
	|Imply (a, b) -> imply_formula ((eval a), (eval b))
	|Equal (a, b) -> less_formula ((calc a), (calc b))
