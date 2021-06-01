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

let rec cal(ex : exp) : int =
match ex with
|Num i -> i
|Plus (ex1, ex2) -> cal(ex1) + cal(ex2)
|Minus (ex1, ex2) -> cal(ex1) - cal(ex2)

let rec eval (f : formula) : bool = 
match f with
|True -> true
|False -> false
|Not f_not -> if eval(f_not) == true then false
  else true
|AndAlso (f1,f2) -> eval(f1) && eval(f2)
|OrElse (f1,f2) -> eval(f1) || eval(f2)
|Imply (f1,f2) -> eval(Not f1) || eval(f2)
|Equal (ex1, ex2) -> if (cal(ex1) = cal(ex2)) then true
        else false 
