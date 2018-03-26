type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula;;

let rec eval : formula -> bool
=fun f -> true;;

let rec eval f = match f with 
  |True -> true
  |False -> false
  |Neg a -> if (a == True) then false else true  
  |Or (a,b) ->( (eval a) || (eval b) )
  |And (a,b) -> ((eval a) && (eval b))
  |Imply (a,b) -> if (( (eval a)==true) && ((eval b)==false) ) then false else true
  |Equiv (a,b) ->  ((eval a)==(eval b));;
