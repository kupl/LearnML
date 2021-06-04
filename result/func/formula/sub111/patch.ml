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
  | Not b -> if eval b = true then false else true
  | Imply (a, b) ->
      if eval a = false then true else if eval b = true then true else false
  | AndAlso (a, b) -> if eval a = true && eval b = true then true else false
  | OrElse (a, b) ->
      if eval a = true then true else if eval b = true then true else false
  | Equal (a, b) ->
      let rec eval_exp (e : exp) : int =
        match e with
        | Num n -> n
        | Plus (n1, n2) -> eval_exp n1 + eval_exp n2
        | Minus (n1, n2) -> eval_exp n1 - eval_exp n2
      in
      if eval_exp a = eval_exp b then true else false
