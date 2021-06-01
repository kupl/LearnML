type exp = Num of int
| Plus of exp * exp
| Minus of exp * exp
;;

type formula = True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
;;

let rec calc e =
        match e with
        Num a -> a
        |Plus(a,b) -> calc a+ calc b
        |Minus(a,b) -> calc a- calc b
;;

let rec eval f =
        match f with
        True -> true
        |False -> false
        |Not a -> if eval a then false else true
        |AndAlso(a,b) -> eval a && eval b
        |OrElse(a,b) -> eval a || eval b
        |Imply(a,b) -> if eval a = true && eval b = false then false else true
        |Equal(c,d) -> calc c = calc d
;;
