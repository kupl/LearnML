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

let rec calc ex =
 match ex with
 | Num n -> n 
 | Plus(n1,n2) -> calc n1 + calc n2 
 | Minus(n1,n2) -> calc n1 - calc n2

let rec eval f=
 match f with
 | True -> true
 | False -> false
 | Not f1 -> if eval f1= true then false else true
 | AndAlso(f1,f2) -> if eval f1 = true then eval f2 else false
 | OrElse(f1,f2) -> if eval f1 = false then eval f2 else true
 | Imply(f1,f2) -> if eval f1 = true then eval f2 else true 
 | Equal(e1,e2) -> if calc e1 = calc e2 then true else false