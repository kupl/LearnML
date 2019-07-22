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

let rec eval p =
  match p with 
    TRUE  -> true
  | FALSE -> false
  | NOT a -> not (eval a)
  | ANDALSO (a, b) -> (eval a) && (eval b)
  | ORELSE  (a, b) -> (eval a) || (eval b) 
  | IMPLY   (a, b) -> (eval (NOT a)) || (eval b)
  | LESS    (a, b) -> if (my_expr a) < (my_expr b) then true else false
and my_expr p =
  match p with
    NUM b -> b
  | PLUS  (a, b) -> (my_expr a) + (my_expr b)
  | MINUS (a, b) -> (my_expr a) - (my_expr b)
