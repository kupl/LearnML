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

let rec eval (f : formula) : bool =
  match f with
  | True  -> true
  | False -> false
  | Not subf -> not (eval subf)
  | AndAlso (sf1, sf2) -> eval sf1 && eval sf2
  | OrElse  (sf1, sf2) -> eval sf1 || eval sf2
  | Imply   (sf1, sf2) -> not (eval sf1) || eval sf2
  | Equal (e1, e2) ->
    let rec evalexp (e : exp) : int =
      match e with
      | Num i -> i
      | Plus  (se1, se2) -> evalexp se1 + evalexp se2
      | Minus (se1, se2) -> evalexp se1 - evalexp se2 in
    evalexp e1 = evalexp e2
