(* 컴퓨터공학부 2013-11425 이창영 hw2_1 *)

type exp = Num of int
| Plus of exp * exp
| Minus of exp * exp

type formula = True
              | False
              | Not of formula
              | AndAlso of formula * formula
              | OrElse of formula * formula
              | Imply of formula * formula
              | Equal of exp * exp

let rec valofexp (e : exp) : int =
  match e with
  | (Num a) -> a
  | (Plus (a, b)) -> (valofexp a) + (valofexp b)
  | (Minus (a, b)) -> (valofexp a) - (valofexp b)

let rec eval (f: formula) : bool =
  match f with
  | True -> true
  | False -> false
  | (Not a) -> not (eval a)
  | (AndAlso (a, b)) -> (eval a) && (eval b)
  | (OrElse (a, b)) -> (eval a) || (eval b)
  | (Imply (a, b)) -> if (eval a) == false then true
                      else (
                        if (eval b) == true then true
                        else false
                        )
  | (Equal (a, b)) -> if (valofexp a) = (valofexp b) then true else false
