type formula = 
  True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp
and exp =
  Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let rec eval : formula->bool =
  fun f ->
    let rec value_of : exp -> int  = 
      fun e ->
        match e with
        | Num i -> i
        | Plus (e1, e2) -> (value_of e1)+(value_of e2)
        | Minus (e1, e2) -> (value_of e1)-(value_of e2)
    in
    match f with 
    | True -> true
    | False -> false
    | Not f_in -> if (eval f_in) then false else true
    | AndAlso (f1,f2) -> (eval f1) && (eval f2)
    | OrElse (f1,f2) -> (eval f1) || (eval f2)
    | Imply (f1,f2) -> 
        if (eval f1)
        then (eval f2)
        else true
    | Equal (e1, e2) -> (value_of e1) = (value_of e2)
        
