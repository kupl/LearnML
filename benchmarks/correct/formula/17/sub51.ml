type formula = True | False | Not of formula
             | AndAlso of formula * formula
             | OrElse of formula * formula
             | Imply of formula * formula
             | Equal of exp * exp
           and exp = Num of int
             | Plus of exp * exp
             | Minus of exp * exp

let rec eval : formula -> bool = fun f ->
  let rec eval_exp : exp -> int = fun e ->
    match e with
    | Num a -> a
    | Plus (b, c) -> (eval_exp b) + (eval_exp c)
    | Minus (d, e) -> (eval_exp d) - (eval_exp e) in
  match f with 
  | True -> true
  | False -> false
  | Not a -> not (eval a)
  | AndAlso (b, c) -> (eval b) && (eval c)
  | OrElse (d, e) -> (eval d) || (eval e)
  | Imply (f, g) -> (
    let eval_f : bool = (eval f) in
    not eval_f || (eval_f && eval g))
  | Equal (h, i) -> ((eval_exp h) = (eval_exp i))


