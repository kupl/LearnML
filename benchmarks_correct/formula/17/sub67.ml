type formula = TRUE | FALSE | NOT of formula | ANDALSO of formula * formula
             | ORELSE of formula * formula | IMPLY of formula * formula
             | LESS of expr * expr
and expr = NUM of int | PLUS of expr * expr | MINUS of expr * expr

let rec expr_to_int (x: expr): int = 
  match x with
  | NUM x -> x
  | PLUS (a, b) -> (expr_to_int a)+(expr_to_int b)
  | MINUS (a, b) -> (expr_to_int a)-(expr_to_int b)

let eval_less ((x: int), (y: int)): bool = x < y

let rec bool_to_formula (b : bool): formula =
  match b with
  | true -> TRUE
  | false -> FALSE

let rec eval (f : formula): bool = 
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT a -> (match a with
              | TRUE -> false
              | FALSE -> true
              | a -> eval (NOT (bool_to_formula (eval a)))
             )
  | ANDALSO (a, b) -> (match (a, b) with
                      | (TRUE, TRUE) -> true
                      | (TRUE, FALSE) -> false
                      | (FALSE, TRUE) -> false
                      | (FALSE, FALSE) -> false
                      | (a, b) -> eval (ANDALSO (bool_to_formula (eval a), bool_to_formula (eval b)))
                      )
  | ORELSE (a, b) -> (match (a, b) with
                     | (TRUE, TRUE) -> true
                     | (TRUE, FALSE) -> true
                     | (FALSE, TRUE) -> true
                     | (FALSE, FALSE) -> false
                     | (a, b) -> eval (ORELSE (bool_to_formula (eval a), bool_to_formula (eval b)))
                     )
  | IMPLY (a, b) -> (match (a, b) with
                    | (TRUE, TRUE) -> true
                    | (FALSE, TRUE) -> true
                    | (TRUE, FALSE) -> false
                    | (FALSE, FALSE) -> true
                    | (a, b) -> eval (IMPLY (bool_to_formula (eval a), bool_to_formula (eval b)))
                    )
  | LESS (c, d) -> eval_less (expr_to_int c, expr_to_int d)
