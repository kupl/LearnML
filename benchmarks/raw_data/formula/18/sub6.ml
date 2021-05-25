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
  
let andAlso (x, y) =  
  if (x = true && y = true) then true else false;;

let orElse (x, y)=
  if (x = false && y = false) then false else true;;

let imply (x, y)=
  if x = false && y = true then true 
  else if x = false && y = false then true
  else if x = true && y = false then false
  else true;;
  
let rec equal (x, y) =
  if x = y then true else false;;
  
let cal (x: exp)=
  match x with
    |Num(n) -> n
    |Plus(Num(a), Num(b)) -> a+b
    |Minus(Num(a), Num(b)) -> a-b
  ;;

let rec eval : formula -> bool
= fun f -> 
  match f with
  | AndAlso(x, y) -> 
    if x = True && y = True then true else if x = False || y = False then false else andAlso(eval x, eval y)
  | OrElse(x , y) -> 
    if x = False && y = False then false else if x = True || y = True then true else orElse(eval x, eval y)
  | Imply(x , y) ->  
    if x = False && y = True then false else if x = False && y = False then true 
    else if x = True && y = False then false else if x = True && y = True then true
    else imply(eval x, eval y)
  | Not(x) ->
    if x = True then false else if x = False then true else not(eval x)
  | True -> true
  | False -> false
  | Equal (x, y) -> if x = y then true else equal(cal x, cal y)
  ;;
  

  eval (Imply (Imply (True,False), True));;
  eval (Equal (Num 1, Plus (Num 1, Num 2)));;



(*
Write the function
eval : formula -> bool
that computes the truth value of a given formula. 

For example,
eval (Imply (Imply (True,False), True))
evaluates to true, and

eval (Equal (Num 1, Plus (Num 1, Num 2)))
evaluates to false
*)