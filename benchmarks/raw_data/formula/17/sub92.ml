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

let rec cal (f : exp) : int =
    match f with
    | Num n -> n
    | Plus (e1, e2) -> (cal e1) + (cal e2)
    | Minus (e1, e2) -> (cal e1) - (cal e2)

let rec eval (f : formula) : bool =
    match f with
    | True -> true
    | False -> false
    | Not (f1) -> if (eval f1) then false
                  else true
    | AndAlso (f1, f2) -> if (eval f1) && (eval f2) then true
                          else false
    | OrElse (f1, f2) -> if (eval (Not f1)) && (eval (Not f2)) then false
                         else true
    | Imply (f1, f2) -> if (eval (Not f1)) then true
                        else if (eval f1) && (eval f2) then true
                        else false
    | Equal (e1, e2) -> if (cal e1) = (cal e2) then true
                       else false