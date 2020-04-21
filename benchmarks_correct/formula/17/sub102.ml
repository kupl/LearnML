(* 2015-11380 박찬양 HW2_1 *)

type formula = True
              | False
              | Not of formula
              | AndAlso of formula * formula
              | OrElse of formula * formula 
              | Imply of formula * formula
              | Equal of exp * exp
    and exp = Num of int
              | Plus of exp * exp
              | Minus of exp * exp

let rec getnum: exp -> int = fun(exp) ->
  match exp with
  | Num a -> a
  | Plus(a,b) -> getnum(a)+getnum(b)
  | Minus(a,b) -> getnum(a)-getnum(b)

let rec eval: formula -> bool = fun(form) ->
  match form with
  | True -> true
  | False -> false
  | Not a ->
        (if eval(a)=true then false
        else true)
  | AndAlso(a,b) -> (eval(a) && eval(b))
  | OrElse(a,b) -> (eval(a) || eval(b))
  | Imply(a,b) -> (not(eval(a)) || eval(b))
  | Equal(a,b) ->
        (if getnum(a) = getnum(b) then true
        else false) 

