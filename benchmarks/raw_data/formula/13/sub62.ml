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

let rec calc a =
        match a with
        | Num a -> a
        | Plus (a, b) -> (calc a)+(calc b)
        | Minus (a, b) -> (calc a)-(calc b)

let rec eval form =
	match form with
	| True -> true
	| False	-> false
	| Not a -> if (eval a) = true then false else true
	| AndAlso (a, b) -> if(eval a && eval b) = true then true else false
	| OrElse (a, b) -> if(eval a || eval b) = true then true else false
	| Imply (a, b) -> if(eval a && not(eval b)) = true then false else true
	| Equal (a, b) -> if(calc a = calc b) = true then true else false

