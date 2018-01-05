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
  | MINUS of expr * expr

let rec eval : formula->bool =
  fun f ->
    let rec value_of : expr -> int  = 
      fun e ->
        match e with
        | NUM i -> i
        | PLUS (e1, e2) -> (value_of e1)+(value_of e2)
        | MINUS (e1, e2) -> (value_of e1)-(value_of e2)
    in
    match f with 
    | TRUE -> true
    | FALSE -> false
    | NOT f_in -> if (eval f_in) then false else true
    | ANDALSO (f1,f2) -> (eval f1) && (eval f2)
    | ORELSE (f1,f2) -> (eval f1) || (eval f2)
    | IMPLY (f1,f2) -> 
        if (eval f1)
        then (eval f2)
        else true
    | LESS (e1, e2) -> (value_of e1) < (value_of e2)
        
