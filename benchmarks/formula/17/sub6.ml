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

let rec expEval = (fun x ->
  match x with
  | PLUS (a,b) -> (expEval a) + (expEval b)
  | MINUS (a,b) -> (expEval a) - (expEval b)
  | NUM a -> a
  )

let rec eval = (fun x ->
  match x with
  | TRUE -> true
  | FALSE -> false
  | NOT a -> not (eval a)
  | ANDALSO (a,b) -> (eval a) && (eval b)
  | ORELSE (a,b) -> (eval a) || (eval b)
  | IMPLY (a,b) -> (
    if (eval a) = true && (eval b) = false
    then false
    else true
    )
  | LESS (a,b) -> (
    if (expEval a) < (expEval b)
    then true
    else false
    )
  )
