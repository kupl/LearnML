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
  let rec eve e =
  match e with
  Num n -> n
|  Plus (e1, e2) -> eve e1 + eve e2
|  Minus (e1, e2) -> eve e1 - eve e2
  let rec eval f =
  match f with
  True -> true
|  False -> false
|  Not f -> not(eval f)
  | AndAlso (f1, f2)-> if(eval f1 && eval f2) then true else false  
| OrElse (f1, f2) -> if(eval f1 || eval f2) then true else false  
| Imply (f1, f2) -> if(eval f1 && eval f2) then true else if (not(eval f1)) then true else false
  | Equal (e1, e2) ->   if(eve e1 = eve e2) then true else false
