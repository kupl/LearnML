type formula = 
  | True
  | False
  | Not of formula
  | AndAlso of formula * formula
  | OrElse of formula * formula
  | Imply of formula * formula
  | Equal of exp * exp

and exp = 
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp

let rec eval : formula -> bool
= fun f ->
  match f with
    | True -> true
    | False -> false
    | Not a -> not (eval a)
    | AndAlso (a, b) -> if ((eval a) = true) && ((eval b) = true) then true else false
    | OrElse (a, b) -> if ((eval a) = true) || ((eval b) = true) then true else false
    | Imply (a, b) -> if ((eval a) = true) && ((eval b) = false) then false else true
    | Equal (a, b) ->
      let rec calc : exp -> int
      = fun e ->
        match e with
          | Num a -> a
          | Plus (a, b) -> (calc a) + (calc b)
          | Minus (a, b) -> (calc a) - (calc b) in
        if (calc a) == (calc b) then true else false ;;