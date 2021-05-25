type formula = True
                      | False
                      | Not of formula
                      | AndAlso of formula * formula
                      | OrElse of formula * formula
                      | Imply of formula * formula
                      | Equal of exp * exp
         and  exp = Num of int
                   | Plus of exp * exp
                   | Minus of exp * exp
        


let rec calex x =
match x with
|Num a-> a
|Plus(a, b) -> (calex a) + (calex b)
|Minus(a, b) -> (calex a) - (calex b)

let rec eval x =
match x with
|True -> true
|False -> false
|Not a->not (eval a)
| AndAlso (a, b) -> (eval a) && (eval b)
| OrElse (a, b) -> (eval a) || (eval b)
| Imply (a, b) -> if (eval a)=false then true
					else (eval b)
| Equal (a, b) -> if (calex a)= (calex b) then true else false
