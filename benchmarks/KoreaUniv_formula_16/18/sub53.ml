type formula =
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp =
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let rec eval : formula -> bool
= fun f -> match f with
  | True -> true | False -> false
  | Not x -> not (eval x)
  | AndAlso (x, y) -> (eval x) && (eval y)
  | OrElse (x, y) -> (eval x) || (eval y)
  | Imply (x, y) -> (not (eval x)) || (eval y)
  | Equal (a, b) ->
    let rec evalexp ex = match ex with
    | Num n -> n
    | Plus (l, r) -> (evalexp l) + (evalexp r)
    | Minus (l, r) -> (evalexp l) - (evalexp r)
    in (evalexp a) = (evalexp b)
;;

eval (Imply (Imply (True,False), True));;
eval (Equal (Num 1, Plus (Num 1, Num 2)));;
