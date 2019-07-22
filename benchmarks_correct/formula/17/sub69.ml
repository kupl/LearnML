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


let rec expr_to_int exp =
  match exp with
  | NUM i -> i
  | PLUS (a, b) -> (expr_to_int a) + (expr_to_int b)
  | MINUS (a, b) -> (expr_to_int a) - (expr_to_int b)

let rec eval fml =
  match fml with
  | TRUE -> true
  | FALSE -> false
  | NOT a -> if (eval a) then false else true
  | ANDALSO (a, b) -> if ((eval a) && (eval b)) then true else false
  | ORELSE (a, b) -> if ((eval a) || (eval b)) then true else false
  | IMPLY (a, b) -> if (not(eval a) || (eval b)) then true else false
  | LESS (a, b) -> if ((expr_to_int a) < (expr_to_int b)) then true else false

