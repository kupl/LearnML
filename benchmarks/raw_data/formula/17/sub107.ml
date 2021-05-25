
(*real code start*)
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

let rec calc (e: exp) : int =
match e with
|Num(x) -> x
|Plus(x,y) -> calc(x) + calc(y)
|Minus(x,y) -> calc(x) - calc(y)

let rec eval (f: formula) : bool =
match f with
|True -> true
|False -> false
|Not(f1) -> if(eval(f1)==true) then false else true
|AndAlso(f1,f2) -> if(eval(f1) == true && eval(f2) == true) then true else false
|OrElse(f1,f2) -> if(eval(f1)==false && eval(f2) == false ) then false else true
|Imply(f1,f2) -> if(eval(f1)==false || eval(f2) == true ) then true else false
|Equal(e1, e2) -> if(calc(e1) = calc(e2)) then true else false
(*real code end*)
