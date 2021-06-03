type exp =
    Num of int
    | Plus of exp * exp
    | Minus of exp * exp
;;

type formula =
    True
    | False
    | Not of formula
    | AndAlso of formula * formula
    | OrElse of formula * formula
    | Imply of formula * formula
    | Equal of exp * exp
;;

let rec compute _exp = match _exp with
    | Plus(e1,e2) -> (compute e1)+(compute e2)
    | Minus(e1,e2) -> (compute e1)-(compute e2)
    | Num x -> x
;;


let rec eval _formula = match _formula with
    | True -> true
    | False -> false
    | Not f -> not (eval f)
    | AndAlso(f1,f2) -> (eval f1) && (eval f2)
    | OrElse(f1,f2) -> (eval f1) || (eval f2)
    | Imply(f1,f2) ->
        if eval f1 then eval f2
        else true
    | Equal(e1,e2) -> (compute e1)=(compute e2)
;;
