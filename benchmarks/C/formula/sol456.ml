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

let rec eval : formula -> bool = fun x ->
  match x with
  | True -> true
  | False -> false
  | Not a ->
    (match eval a with
    | true -> false
    | false -> true)
  | AndAlso (a , b) ->
    (match ( eval a , eval b) with
    | (true , true) -> true
    | (true , false) -> false
    | (false , _ ) -> false )
  | OrElse (a , b) ->
    (match (eval a, eval b) with
    | (true , _) -> true
    | (false , true) -> true
    | (false , false) -> false)
  | Imply (a,b) ->
    (match (eval a, eval b) with
    | (true , true) -> true
    | (true , false) -> false
    | (false , _ ) -> true)
  | Equal (x , y) ->
    let rec real : exp -> int = fun x ->
      match x with 
      | Num i -> i
      | Plus (k,j) -> (real k) + (real j)
      | Minus (k,j) -> (real k) - (real j) in
    if real x = real y then true
    else false