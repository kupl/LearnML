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

let rec eval form = 
  let rec calculate e = 
    match e with
    | Num(i) -> i
    | Plus(a, b) -> (calculate a) + (calculate b)
    | Minus(a, b) -> (calculate a) - (calculate b)
  in

  match form with
  | True -> true
  | False -> false
  | Not(a) -> not (eval a)
  | AndAlso(a, b) -> (eval a) && (eval b)
  | OrElse(a, b) -> (eval a) || (eval b)
  | Imply(a, b) -> (
    if (eval a) = true then
      (eval b)
    else
      true
  )
  | Equal(a, b) -> (calculate a) = (calculate b)
