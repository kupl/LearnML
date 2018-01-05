type formula = 
    TRUE
    | FALSE
    | NOT of formula
    | ANDALSO of formula * formula
    | ORELSE of formula * formula
    | IMPLY of formula * formula
    | LESS of expr * expr
and expr = 
    NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr;;

let rec val_of_expr = function
    | NUM v -> v
    | PLUS (e1, e2) ->
            let v1 = (val_of_expr e1) in
            let v2 = (val_of_expr e2) in
            v1 + v2
    | MINUS (e1, e2) ->
            let v1 = (val_of_expr e1) in
            let v2 = (val_of_expr e2) in
            v1 - v2;;    

let rec eval = function
    | TRUE -> true
    | FALSE -> false
    | NOT p -> (not (eval p))
    | ANDALSO (p1, p2) -> ((eval p1) && (eval p2))
    | ORELSE (p1, p2) -> ((eval p1) || (eval p2))
    | IMPLY (p1, p2) -> 
            let b1 = (eval p1) in
            let b2 = (eval p2) in
            ((not b1) || b2)
    | LESS (e1, e2) ->
            let v1 = (val_of_expr e1) in
            let v2 = (val_of_expr e2) in
            (v1 < v2);; 
