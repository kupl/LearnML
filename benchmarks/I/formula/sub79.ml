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


let rec plmi : exp -> int = fun exp ->
  match exp with
    Num n -> n
  | Plus (n1, n2) -> plmi n1 + plmi n2
  | Minus (n1, n2) -> plmi n1 - plmi n2

let eval : formula -> bool = fun fm ->
  match fm with 
    True -> true
  | False -> false
  | Not fm1 -> if fm1 = True then false  else true
  | AndAlso (fm1, fm2)  -> if fm1 = False then false
                           else (if fm2 = False then false else true)
  | OrElse (fm1, fm2) -> if fm1 = True then true
                         else (if fm2 = True then true else false)
  | Imply (fm1, fm2) -> if (fm1=True && fm2=False) then false
                        else true
  | Equal (exp1, exp2) -> if( (plmi exp1) = (plmi exp2)) then true else false
