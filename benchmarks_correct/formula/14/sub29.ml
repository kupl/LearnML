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

let rec evalexp (f: expr): int =
  match f with
    | NUM a -> a
    | PLUS (a, b) -> evalexp a + evalexp b
    | MINUS (a, b) -> evalexp a - evalexp b

let rec eval (f: formula): bool =
  match f with
    | TRUE -> true
    | FALSE -> false
    | NOT a -> if (eval a) = true then false else true
    | ANDALSO (a, b) -> eval a && eval b
    | ORELSE (a, b) -> eval a || eval b
    | IMPLY (a, b) -> not (eval a) || eval b
    | LESS (a, b) -> if (evalexp a) < (evalexp b) then true else false



  