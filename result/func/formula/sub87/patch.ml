type formula =
  | True
  | False
  | Not of formula
  | AndAlso of (formula * formula)
  | OrElse of (formula * formula)
  | Imply of (formula * formula)
  | Equal of (exp * exp)

and exp = Num of int | Plus of (exp * exp) | Minus of (exp * exp)

let rec eval (f : formula) : bool =
  match f with
  | True -> true
  | False -> false
  | Not p -> if eval p then false else true
  | AndAlso (p, q) -> if eval p then if eval q then true else false else false
  | OrElse (p, q) -> if p = True then true else if eval q then true else false
  | Imply (p, q) -> if eval p then if eval q then true else false else true
  | Equal (n1, n2) ->
      let rec exp_to_int (e : exp) : int =
        match e with
        | Num n -> n
        | Plus (n1, n2) -> exp_to_int n1 + exp_to_int n2
        | Minus (n1, n2) -> exp_to_int n1 - exp_to_int n2
      in
      if exp_to_int n1 = exp_to_int n2 then true else false


let (_ : bool) = eval (Imply (Imply (True, False), True))

let (_ : bool) = eval (Equal (Num 1, Plus (Num 1, Num 2)))
