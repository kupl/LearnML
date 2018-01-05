(* 컴퓨터공학부 2013-11425 이창영 hw2_1 *)

type expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr

type formula = TRUE
              | FALSE
              | NOT of formula
              | ANDALSO of formula * formula
              | ORELSE of formula * formula
              | IMPLY of formula * formula
              | LESS of expr * expr

let rec valofexpr (e : expr) : int =
  match e with
  | (NUM a) -> a
  | (PLUS (a, b)) -> (valofexpr a) + (valofexpr b)
  | (MINUS (a, b)) -> (valofexpr a) - (valofexpr b)

let rec eval (f: formula) : bool =
  match f with
  | TRUE -> true
  | FALSE -> false
  | (NOT a) -> not (eval a)
  | (ANDALSO (a, b)) -> (eval a) && (eval b)
  | (ORELSE (a, b)) -> (eval a) || (eval b)
  | (IMPLY (a, b)) -> if (eval a) == false then true
                      else (
                        if (eval b) == true then true
                        else false
                        )
  | (LESS (a, b)) -> if (valofexpr a) < (valofexpr b) then true else false
