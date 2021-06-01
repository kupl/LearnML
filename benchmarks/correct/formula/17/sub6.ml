type formula = True
              | False
              | Not of formula
              | AndAlso of formula * formula
              | OrElse of formula * formula
              | Imply of formula * formula
              | Equal of exp * exp

and exp = Num of int
          | Plus of exp * exp
          | Minus of exp * exp

let rec expEval = (fun x ->
  match x with
  | Plus (a,b) -> (expEval a) + (expEval b)
  | Minus (a,b) -> (expEval a) - (expEval b)
  | Num a -> a
  )

let rec eval = (fun x ->
  match x with
  | True -> true
  | False -> false
  | Not a -> not (eval a)
  | AndAlso (a,b) -> (eval a) && (eval b)
  | OrElse (a,b) -> (eval a) || (eval b)
  | Imply (a,b) -> (
    if (eval a) = true && (eval b) = false
    then false
    else true
    )
  | Equal (a,b) -> (
    if (expEval a) = (expEval b)
    then true
    else false
    )
  )
