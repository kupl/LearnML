(* 2015-11380 박찬양 HW2_1 *)

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

let rec getnum: expr -> int = fun(exp) ->
  match exp with
  | NUM a -> a
  | PLUS(a,b) -> getnum(a)+getnum(b)
  | MINUS(a,b) -> getnum(a)-getnum(b)

let rec eval: formula -> bool = fun(form) ->
  match form with
  | TRUE -> true
  | FALSE -> false
  | NOT a ->
        (if eval(a)=true then false
        else true)
  | ANDALSO(a,b) -> (eval(a) && eval(b))
  | ORELSE(a,b) -> (eval(a) || eval(b))
  | IMPLY(a,b) -> (not(eval(a)) || eval(b))
  | LESS(a,b) ->
        (if getnum(a) < getnum(b) then true
        else false) 

