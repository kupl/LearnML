type formula =
| True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp

and exp =
| Num of int
| Plus of exp * exp
| Minus of exp * exp

let rec eval : formula -> bool
= fun f ->
let rec change : exp -> int
 = fun ex ->
match ex with
|Num a -> a
|Plus(a, b) -> (change a) + (change b)
|Minus(a, b) -> (change a) - (change b)
in
match f with
|True -> true
|False -> false
|Not m -> if eval m = true then false else true
|AndAlso(m, n) -> if eval m = true && eval n = true then true else false
|OrElse(m, n) -> if  eval m = true || eval n = true then true else false
|Imply(m, n) -> if eval m = true  && eval n = false then false
else true
|Equal(a, b) -> if (change a) = (change b) then true else false
;;
