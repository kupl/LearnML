exception TODO

type formula = 
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec eval1 (f1: expr): int =
  match f1 with
  | NUM n -> n
  | PLUS (a, b) -> (eval1 a) + (eval1 b)
  | MINUS (a, b) -> (eval1 a) - (eval1 b)

let rec eval (f: formula): bool =
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT n -> not (eval n)
  | ANDALSO (a, b) -> (eval a) && (eval b)
  | ORELSE (a, b) -> (eval a) || (eval b)
  | IMPLY (a, b) -> if (eval a) && (eval b) then true
		    else if (eval a) && not (eval b) then false
		    else if not (eval a) && (eval b) then true
		    else true
  | LESS (a, b) -> (eval1 a) < (eval1 b)



