(* hw1ex4 "TrueFalse" *)

type formula = TRUE
             | FALSE
             | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr
and expr = NUM of int
         | PLUS of expr * expr
         | MINUS of expr * expr


let rec calc (e: expr) : int = 
  match e with
    | NUM i -> i
    | PLUS (e1,e2) -> (calc e1) + (calc e2)
    | MINUS (e1,e2) -> (calc e1) + (calc e2)


let rec eval (f: formula) : bool = 
  match f with
    | TRUE -> true
    | FALSE -> false
    | NOT subf -> 
        if (eval subf) = true then false
        else true
    | ANDALSO (f1, f2) ->
        if (eval f1) && (eval f2) then true
        else false
    | ORELSE (f1, f2) ->
        if (eval f1) || (eval f2) then true
        else false
    | IMPLY (f1, f2) ->
        if not (eval f1) then true
        else if (eval f2) then true
        else false
    | LESS (e1, e2) ->
        if (calc e1) < (calc e2) then true
        else false

