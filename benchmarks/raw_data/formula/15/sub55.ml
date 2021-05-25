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

let rec value : exp -> int = fun l ->
 match l with
 | Num n -> n
 | Plus(m,n) -> (value m) + (value n)
 | Minus(m,n) -> (value m) - (value n)

let rec eval : formula -> bool = fun form ->
 match form with
 | True -> true
 | False -> false
 | Not x -> not (eval x)
 | AndAlso (x,y) -> (eval x)&&(eval y)
 | OrElse (x,y) -> (eval x)||(eval y)
 | Imply (x,y) -> (not (eval x))||(eval y)
 | Equal (x,y) -> (value x)=(value y)