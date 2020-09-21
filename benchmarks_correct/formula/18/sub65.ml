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
  let rec exp2int
      = fun g ->
        match g with
          | Num a -> a
          | Plus (a, b) -> exp2int a + exp2int b
          | Minus (a, b) -> exp2int a - exp2int b
  in
  match f with
    | True -> true
    | False -> false 
    | Not p -> not (eval p)
    | AndAlso (p, q) -> (eval p) && (eval q)
    | OrElse (p, q) -> (eval p) || (eval q)
    | Imply (p, q) -> (not (eval p)) || (eval q)
    | Equal (p, q) -> exp2int p = exp2int q
    ;;
    