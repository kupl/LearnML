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

let rec eval form = 
  let rec calculate e = 
    match e with
    | NUM(i) -> i
    | PLUS(a, b) -> (calculate a) + (calculate b)
    | MINUS(a, b) -> (calculate a) - (calculate b)
  in

  match form with
  | TRUE -> true
  | FALSE -> false
  | NOT(a) -> not (eval a)
  | ANDALSO(a, b) -> (eval a) && (eval b)
  | ORELSE(a, b) -> (eval a) || (eval b)
  | IMPLY(a, b) -> (
    if (eval a) = true then
      (eval b)
    else
      true
  )
  | LESS(a, b) -> (calculate a) < (calculate b)
