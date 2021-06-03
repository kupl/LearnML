(*컴퓨터공학부 2014-16775 김민지
priogramming language hw 2-1*)

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

let rec calcul (x:exp) : int =
  match x with
  |Num (n) -> n
  |Plus (e1, e2) -> (calcul e1) + (calcul e2)
  |Minus (e1, e2) -> (calcul e1) - (calcul e2)

let rec eval (x:formula) : bool = 
  match x with
  |True -> true
  |False -> false
  |Not x1 -> not (eval x1)
  |AndAlso (x1, x2) ->
    (match ((eval x1), (eval x2)) with
      |(true, true) -> true
      |(true, false) -> false
      |(false, _) -> false
    )
  |OrElse (x1, x2) ->
    (match ((eval x1), (eval x2)) with
      |(false, false) -> false
      |(false, true) ->true
      |(true, _) -> true
    )
  |Imply (x1, x2) ->
    (match ((eval x1), (eval x2)) with
      |(true, true) -> true
      |(true, false) -> false
      |(false, _) -> true
    )
  |Equal (x1, x2) ->
    if ((calcul x1) = (calcul x2)) then true else false

