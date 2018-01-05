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

let rec value : expr -> int = fun a ->
  match a with
  | NUM a -> a
  | PLUS(a,b) -> value a + value b
  | MINUS(a,b) -> value a - value b

let eval : formula -> bool = fun f -> 
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT a ->
      (
        if a=TRUE then false
        else true
      )
  | ANDALSO (a, b) ->
      (
        if a=FALSE then false
        else if b=FALSE then false
        else true
      )
  | ORELSE (a, b) ->
      (
        if a=TRUE then true
        else if b=TRUE then true
        else false
      )
  | IMPLY (a, b) ->
      (
        if a=FALSE then true
        else if b=TRUE then true
        else false
      )
  | LESS (a,b) ->
      (if value a < value b then true
       else false
      )
