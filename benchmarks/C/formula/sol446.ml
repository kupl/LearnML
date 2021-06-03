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

let rec expval : exp -> int = fun f ->
    match f with
    |Num a -> a
    |Plus (a,b) -> expval(a) + expval(b)
    |Minus (a,b) -> expval(a) - expval(b)

let rec eval : formula -> bool = fun f ->
    match f with
    |True -> true
    |False -> false
    |Not g -> 
        if(eval(g)) then false
        else true
    |AndAlso (a,b) -> eval(a) && eval(b)
    |OrElse (a,b) -> eval(a) || eval(b)
    |Imply (a,b) -> eval(Not a) || eval(b)
    |Equal (x,y) -> expval(x) = expval(y)