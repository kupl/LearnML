type formula = TRUE
               | FALSE
               | NOT of formula
               | ANDALSO of formula * formula
               | ORELSE of formula * formula
               | IMPLY of formula * formula
               | LESS of expr * expr
and expr = NUM of int
           | PLUS of expr * expr
           | MINUS of expr * expr;;

let rec eval_expr = function
  | NUM a -> a
  | PLUS (a, b) -> eval_expr a + eval_expr b
  | MINUS (a, b) -> eval_expr a - eval_expr b;;

let rec eval = function
  | TRUE -> true
  | FALSE -> false
  | NOT a -> if (eval a) = true then false else true
  | ANDALSO (a, b) -> if (eval a) = true && (eval b) = true then true else false
  | ORELSE (a, b) -> if (eval a) = true || (eval b) = true then true else false
  | IMPLY (a, b) -> if (eval a) = true && (eval b) = false then false else true
  | LESS (a, b) -> if (eval_expr a) < (eval_expr b) then true else false;;
