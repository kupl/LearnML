type formula = True | False | Not of formula
| AndAlso of formula * formula | OrElse of formula * formula
| Imply of formula * formula | Equal of exp * exp
and exp = Num of int | Plus of exp * exp | Minus of exp * exp

let rec eval : formula -> bool = fun x -> match x with
| True -> true
| False -> false
| Not(f) -> not(eval f)
| AndAlso(a, b) -> ( eval a && eval b )
| OrElse(a, b) -> ( eval a || eval b )
| Imply(a, b) -> ( not (eval a) || (eval b) )
| Equal(a, b) -> if ( ( evalExpr a ) = ( evalExpr b ) ) then true else false
and evalExpr ( x : exp ) = match x with
| Num(i) -> i
| Plus(a, b) -> ( evalExpr a ) + ( evalExpr b )
| Minus(a, b) -> ( evalExpr a ) - ( evalExpr b )
