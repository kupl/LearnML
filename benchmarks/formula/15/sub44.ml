type formula = TRUE | FALSE
             | NOT of formula
             | ANDALSO of formula * formula
             | ORELSE of formula * formula
             | IMPLY of formula * formula
             | LESS of expr * expr
and expr = NUM of int
         | PLUS of expr * expr
         | MINUS of expr * expr

let rec calc i =
  match i with
  | NUM a -> a
  | PLUS (a, b) -> (calc a) + (calc b)
  | MINUS (a, b) -> (calc a) - (calc b)

let rec eval f =
  match f with
  | TRUE -> true
  | FALSE -> false
  | ANDALSO (a, b) -> (eval a) && (eval b)
  | ORELSE (a, b) -> (eval a) || (eval b)
  | IMPLY (a, b) -> (not (eval a)) || (eval b)
  | NOT a -> not (eval a)
  | LESS (a, b) -> (calc a) < (calc b)
