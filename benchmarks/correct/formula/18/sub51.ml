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
  let rec compute exp =
    match exp with
       Num n -> n
      |Plus (exp1, exp2) -> (compute exp1) + (compute exp2)
      |Minus (exp1, exp2) -> (compute exp1) - (compute exp2)
  in
    match f with
       True -> true
      |False -> false
      |Not f -> not (eval f)
      |AndAlso (f1, f2) -> (eval f1) && (eval f2)
      |OrElse (f1, f2) -> (eval f1) || (eval f2)
      |Equal (exp1, exp2) -> (compute exp1) == (compute exp2)
      |Imply (f1, f2) ->
        match (eval f1),(eval f2) with
           true, false -> false
          |_, _ -> true;;
          
eval (Imply (Imply (True,False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;