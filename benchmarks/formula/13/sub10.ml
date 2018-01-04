(* ID : 2007-12138 *)

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


(* Simple implementation based on the truth table of each logical operation *)
let rec eval f =
  match f with
  | TRUE -> TRUE
  | FALSE -> FALSE
  | NOT(p) -> 
     if p = TRUE then FALSE
     else TRUE
  | ANDALSO(p,q) ->
     if (p=TRUE & q=TRUE) then TRUE
     else FALSE
  | ORELSE(p,q) ->
     if (p=FALSE & q=FALSE) then FALSE
     else TRUE
  | IMPLY(p,q) ->
     if q = TRUE then TRUE
     else if q = TRUE then TRUE
     else FALSE
  | LESS(p,q) ->
     if (p<q) then TRUE
     else FALSE

