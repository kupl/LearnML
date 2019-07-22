type formula =
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec num : expr -> int = fun x ->
  match x with
  | NUM n -> n
  | PLUS (n1, n2) -> (num n1) + (num n2)
  | MINUS (n1, n2) -> (num n1) - (num n2)

let rec eval : formula -> bool = fun x ->
  match x with
  | TRUE -> true
  | FALSE -> false
  | NOT y -> not (eval y)
  | ANDALSO (a, b) -> (eval a) && (eval b)
  | ORELSE (a, b) -> (eval a) || (eval b)
  | IMPLY (a, b) -> (not (eval a)) || (eval b)
  | LESS (a, b) -> (num a) < (num b)
