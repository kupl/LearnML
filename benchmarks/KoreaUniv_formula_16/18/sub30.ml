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
  match f with
    |True -> true
    |False -> false
    |Not (sub_f) -> not (eval sub_f)
    |AndAlso (f1, f2) -> (eval f1) && (eval f2)
    |OrElse (f1, f2) -> (eval f1) || (eval f2)
    |Imply (f1, f2) -> 
      if ((eval f1) = true) && ((eval f2) = false) then false
        else true
    |Equal (exp1, exp2) ->
      let rec find_exp ex =
        match ex with
          |Num(n) -> n
          |Plus (n1,n2) -> (find_exp n1) + (find_exp n2)
          |Minus (n1, n2) -> (find_exp n1) - (find_exp n2)
      in if(find_exp exp1) = (find_exp exp2) then true
      else false;;
    

