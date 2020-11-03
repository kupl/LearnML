type formula = True
|False
|Not of formula
|AndAlso of formula * formula
|OrElse of formula * formula
|Imply of formula * formula
|Equal of exp * exp
and exp = Num of int
|Plus of exp * exp
|Minus of exp * exp

let rec eval : formula -> bool = fun form ->
match form with
|True -> true
|False -> false
|Not form -> not (eval form)
|AndAlso (f1,f2) -> if eval f1 && eval f2 then true else false
|OrElse (f1,f2) -> if eval f1 || eval f2 then true else false
|Imply (f1,f2) -> if eval f1 && not (eval f2) then false else true
|Equal (e1,e2) -> if calc e1 = calc e2 then true else false
and calc : exp -> int = fun ex ->
match ex with
|Num integr -> integr
|Plus (e1,e2) -> calc e1 + calc e2
|Minus (e1,e2) -> calc e1 - calc e2
