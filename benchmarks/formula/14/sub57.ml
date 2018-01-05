type expr =
    NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr
;;

type formula =
    TRUE
    | FALSE
    | NOT of formula
    | ANDALSO of formula * formula
    | ORELSE of formula * formula
    | IMPLY of formula * formula
    | LESS of expr * expr
;;

let rec compute _expr = match _expr with
    | PLUS(e1,e2) -> (compute e1)+(compute e2)
    | MINUS(e1,e2) -> (compute e1)-(compute e2)
    | NUM x -> x
;;


let rec eval _formula = match _formula with
    | TRUE -> true
    | FALSE -> false
    | NOT f -> not (eval f)
    | ANDALSO(f1,f2) -> (eval f1) && (eval f2)
    | ORELSE(f1,f2) -> (eval f1) || (eval f2)
    | IMPLY(f1,f2) ->
        if eval f1 then eval f2
        else true
    | LESS(e1,e2) -> (compute e1)<(compute e2)
;;
