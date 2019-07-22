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

let rec eval s = 
    match s with
    | TRUE -> true
    | FALSE -> false
    | NOT (a) -> not (eval a)
    | ANDALSO (a,b) -> eval (a) && eval (b)
    | ORELSE (a,b) -> eval (a) || eval (b)
    | IMPLY (a,b) -> if eval (a) then eval (b) else true
    | LESS (a,b) ->
        let rec eval2 e =
          match e with
          | NUM (n) -> n
          | PLUS (e1, e2) -> eval2 (e1) + eval2 (e2)
          | MINUS (e1, e2) -> eval2 (e1) - eval2 (e2)
        in
  	if eval2 (a) < eval2 (b) then true else false;;
