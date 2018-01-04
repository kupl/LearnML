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

let rec eval : formula -> bool = fun x ->
  match x with
  | TRUE -> true
  | FALSE -> false
  | NOT a ->
    (match eval a with
    | true -> false
    | false -> true)
  | ANDALSO (a , b) ->
    (match ( eval a , eval b) with
    | (true , true) -> true
    | (true , false) -> false
    | (false , _ ) -> false )
  | ORELSE (a , b) ->
    (match (eval a, eval b) with
    | (true , _) -> true
    | (false , true) -> true
    | (false , false) -> false)
  | IMPLY (a,b) ->
    (match (eval a, eval b) with
    | (true , true) -> true
    | (true , false) -> false
    | (false , _ ) -> true)
  | LESS (x , y) ->
    let rec real : expr -> int = fun x ->
      match x with 
      | NUM i -> i
      | PLUS (k,j) -> (real k) + (real j)
      | MINUS (k,j) -> (real k) - (real j) in
    if real x < real y then true
    else false