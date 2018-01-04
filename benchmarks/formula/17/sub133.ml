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


let rec plmi : expr -> int = fun exp ->
  match exp with
    NUM n -> n
  | PLUS (n1, n2) -> plmi n1 + plmi n2
  | MINUS (n1, n2) -> plmi n1 - plmi n2

let eval : formula -> bool = fun fm ->
  match fm with 
    TRUE -> true
  | FALSE -> false
  | NOT fm1 -> if fm1 = TRUE then false  else true
  | ANDALSO (fm1, fm2)  -> if fm1 = FALSE then false
                           else (if fm2 = FALSE then false else true)
  | ORELSE (fm1, fm2) -> if fm1 = TRUE then true
                         else (if fm2 = TRUE then true else false)
  | IMPLY (fm1, fm2) -> if (fm1=TRUE && fm2=FALSE) then false
                        else true
  | LESS (exp1, exp2) -> if( (plmi exp1) < (plmi exp2)) then true else false
