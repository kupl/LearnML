(*컴퓨터공학부 2014-16775 김민지
priogramming language hw 2-1*)

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

let rec calcul (x:expr) : int =
  match x with
  |NUM (n) -> n
  |PLUS (e1, e2) -> (calcul e1) + (calcul e2)
  |MINUS (e1, e2) -> (calcul e1) - (calcul e2)

let rec eval (x:formula) : bool = 
  match x with
  |TRUE -> true
  |FALSE -> false
  |NOT x1 -> not (eval x1)
  |ANDALSO (x1, x2) ->
    (match ((eval x1), (eval x2)) with
      |(true, true) -> true
      |(true, false) -> false
      |(false, _) -> false
    )
  |ORELSE (x1, x2) ->
    (match ((eval x1), (eval x2)) with
      |(false, false) -> false
      |(false, true) ->true
      |(true, _) -> true
    )
  |IMPLY (x1, x2) ->
    (match ((eval x1), (eval x2)) with
      |(true, true) -> true
      |(true, false) -> false
      |(false, _) -> true
    )
  |LESS (x1, x2) ->
    if ((calcul x1) < (calcul x2)) then true else false

