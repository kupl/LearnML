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
    | Minus of exp * exp;;

let rec val_of_exp = function
    | Num v -> v
    | Plus (e1, e2) ->
            let v1 = (val_of_exp e1) in
            let v2 = (val_of_exp e2) in
            v1 + v2
    | Minus (e1, e2) ->
            let v1 = (val_of_exp e1) in
            let v2 = (val_of_exp e2) in
            v1 - v2;;    

let rec eval = function
    | True -> true
    | False -> false
    | Not p -> (not (eval p))
    | AndAlso (p1, p2) -> ((eval p1) && (eval p2))
    | OrElse (p1, p2) -> ((eval p1) || (eval p2))
    | Imply (p1, p2) -> 
            let b1 = (eval p1) in
            let b2 = (eval p2) in
            ((not b1) || b2)
    | Equal (e1, e2) ->
            let v1 = (val_of_exp e1) in
            let v2 = (val_of_exp e2) in
            (v1 = v2);; 
